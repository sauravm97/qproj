let indent = "  "

protocol Lines: CustomStringConvertible {
  var lines: [String] { get }
}

extension Lines {
  var description: String { return lines.joined(separator: "\n") }
}

extension Array: Lines where Element: Lines {
  var lines: [String] {
    return map { $0.lines.map { indent + $0 } }.joined(separator: [","]).map { $0 }
  }
}

extension Subject: Lines {
  var lines: [String] {
    switch self {
    case .value(let value):
      return value.atom.lines
    case .variable(let variable):
      return ["Variable: \(variable.name)"]
    }
  }
}

extension Atom: Lines {
  var lines: [String] {
    if subjects.count == 0 {
      return ["Atom: \(predicate)"]
    }
    return ["Atom: \(predicate)("] + subjects.lines.map { indent + $0 } + [")"]
  }
}

extension Body: Lines {
  var lines: [String] {
    return ["Body"] + terms.flatMap { $0.lines }.map { indent + $0 }
  }
}

extension Sentence: Lines {
  var lines: [String] {
    return ["Sentence"] + ["head: \(head)", "body: \(String(describing: body))"].map { indent + $0 }
  }
}

extension KnowledgeBase: Lines {
  var lines: [String] {
    return ["Program"] + sentences.flatMap { $0.lines }.map { indent + $0 }
  }
}

extension Query: Lines {
  var lines: [String] {
    return ["Query"] + terms.flatMap { $0.lines }.map { indent + $0 }
  }
}
