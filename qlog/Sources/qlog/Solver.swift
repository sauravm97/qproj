enum Result<Y, N> {
  case yes(Y)
  case no(N)
}

func solve(kb: String, query: String, failure: @escaping ([Exception]) -> Void, success: @escaping (Result<(Atom, Substitution), Atom>) -> Void) {
  KnowledgeBase.parse(tokens: tokenise(code: kb), failure: failure) { kb, tokens in
    Query.parse(tokens: tokenise(code: query), failure: failure) { query, tokens in
      let goals = Set<Atom>(query.terms)
      let goalVariables = variables(goals)
      let yes = Atom.yes(subjects: goalVariables.map { .variable($0) })
      if let substitution = search(kb: kb, goals: goals) {
        success(.yes((yes, substitution.filter { goalVariables.contains($0.key) })))
      } else {
        success(.no(Atom.no))
      }
    }
  }
}
