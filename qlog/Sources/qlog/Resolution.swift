//typealias Daughter = Female
//typealias Mother = Female
//typealias Father = Male
//typealias Husband = Male
//
//typealias Unification = (toWife: [Variable: Subject], toHusband: [Variable: Value])
//
//enum Status {
//  case unknown
//  case failed
//  case forcedFailure
//  case resolved
//}
//
//struct Female: Goal {
//  var status: Status = .unknown
//
//  let atom: Atom
//  let father: Father?
//  var husbands: [Father]
//  var currentHusband: Int
//
//  init(atom: Atom, father: Father? = nil, husbands: [Husband]) {
//    self.atom = atom
//    self.father = father
//    self.husbands = husbands
//    self.currentHusband = 0
//  }
//
//  mutating func redo() {
//    guard currentHusband > 0 else {
//      return
//    }
//    status = .unknown
//    let previousHusband = currentHusband - 1
//    husbands[previousHusband].redo()
//  }
//}
//
//struct Male: Goal {
//  var status: Status = .unknown
//
//  let sentence: Sentence
//  let unificationWithWife: Unification
//  var daughters: [Daughter]?
//  var currentDaughter: Int
//
//  init(sentence: Sentence, unificationWithWife: Unification) {
//    self.sentence = sentence
//    self.unificationWithWife = unificationWithWife
//
//    self.daughters = nil
//    self.currentDaughter = 0
//  }
//
//  mutating func createDaughters(withKB kb: KnowledgeBase) {
//    daughters = sentence.body.terms.map { atom in
//      let husbands: [Husband] = kb.sentences.compactMap { sentence in
//        guard let unification = unify(sentence.head, with: atom) else {
//          return nil
//        }
//        return Husband(sentence: sentence, unificationWithWife: unification)
//      }
//      return Daughter(atom: atom, father: self, husbands: husbands)
//    }
//  }
//
//  mutating func redo() {
//    status = .unknown
//    let previousDaughter = currentDaughter - 1
//    daughters?[previousDaughter].redo()
//  }
//}
//
//protocol Goal {
//  var status: Status { get }
//  mutating func redo()
//  func call()
//  func exit()
//  func fail()
//}

func selectGoal(fromGoals goals: Set<Atom>) -> Atom? {
  return goals.first
}

func isGoalNode(state: Sentence) -> Bool {
  return state.head.predicate == "yes" && state.body.terms.isEmpty
}

func unify(_ atom: Atom, with goal: Atom) -> (atomToGoal: [Variable: Subject], goalToAtom: [Variable: Value])? {
  guard atom.predicate == goal.predicate else {
    return nil
  }
  var atomToGoal: [Variable: Subject] = [:]
  var goalToAtom: [Variable: Value] = [:]
  for (atomSubject, goalSubject) in zip(atom.subjects, goal.subjects) {
    if case let .value(atomSubjectValue) = atomSubject, case let .value(goalSubjectValue) = goalSubject {
      guard
        let subunification = unify(atomSubjectValue.atom, with: goalSubjectValue.atom),
        let _ = try? atomToGoal.merge(subunification.atomToGoal, uniquingKeysWith: {
          s1, s2 in
          guard s1 == s2 else {
            throw Exception(description: "")
          }
          return s1
        }),
        let _ = try? goalToAtom.merge(subunification.goalToAtom, uniquingKeysWith: {
          v1, v2 in
          guard v1 == v2 else {
            throw Exception(description: "")
          }
          return v1
        }) else {
          return nil
      }
    } else if case let .variable(atomSubjectVariable) = atomSubject {
      atomToGoal[atomSubjectVariable] = goalSubject
    } else if case let .variable(goalSubjectVariable) = goalSubject, case let .value(atomSubjectValue) = atomSubject {
      goalToAtom[goalSubjectVariable] = atomSubjectValue
    }
  }
  return (atomToGoal: atomToGoal, goalToAtom: goalToAtom)
}

func variables<T: Sequence>(_ atoms: T) -> Set<Variable> where T.Element == Atom {
  return Set(atoms.flatMap { $0.subjects }.compactMap {
    if case let .variable(variable) = $0 {
      return variable
    }
    return nil
  })
}

var counter = 0
func uniqueVariable() -> Variable {
  let name = "temp\(counter)"
  counter += 1
  return Variable(name: name)
}

func search(kb: KnowledgeBase, goals: Set<Atom>) -> Substitution? {
  if goals.isEmpty {
    return [:]
  }

  guard let goal = selectGoal(fromGoals: goals) else {
    return nil
  }
  let goals = goals.subtracting([goal])

  for sentence in kb.sentences {
    guard let (atomToGoal, goalToAtom) = unify(sentence.head, with: goal) else {
      continue
    }
    let goalVariables = variables(goals)
    let nameClashes = variables(sentence.body.terms).subtracting(variables([sentence.head])).intersection(goalVariables)
    let renamings = Dictionary(uniqueKeysWithValues: nameClashes.map { ($0, uniqueVariable()) })
    let terms = sentence.body.terms.map { atom in
      Atom(predicate: atom.predicate, subjects: atom.subjects.map {
        if case let .variable(subjectVariable) = $0, let subject = atomToGoal[subjectVariable] {
          return subject
        }
        return $0
      })
    }
    let body = Body(terms: terms.map { atom in
      Atom(predicate: atom.predicate, subjects: atom.subjects.map {
        if case let .variable(subjectVariable) = $0, let variable = renamings[subjectVariable] {
          return .variable(variable)
        }
        return $0
      })
    })
    let goals = Set(goals.map { atom in
      Atom(predicate: atom.predicate, subjects: atom.subjects.map {
        if case let .variable(subjectVariable) = $0, let value = goalToAtom[subjectVariable] {
          return .value(value)
        }
        return $0
      })
    }).union(body.terms)
    if let substitution = search(kb: kb, goals: goals) {
      let goalVariables = variables(goals)
      return goalToAtom.merging(substitution.filter { goalVariables.contains($0.key) }, uniquingKeysWith: { _,_ in fatalError() })
    }
  }
  return nil
}
