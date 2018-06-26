enum Subject: Parseable, Hashable {
  case variable(Variable)
  case value(Value)

  static func parse<T: Collection>(tokens: T, failure: @escaping ([Exception]) -> Void, success: @escaping (Subject, T.SubSequence) -> Void) where T.Element == Token, T.Index == Int {
    Atom.parse(tokens: tokens, failure: { _ in
      String.parse(tokens: tokens, failure: { failure($0 + [Exception(description: "Subject")]) }) { text, tokens in
        if text.prefix(1) + text.dropFirst().lowercased() == text {
          success(.variable(Variable(name: text)), tokens)
        } else {
          failure([Exception(description: "Subject")])
        }
      }
    }) { atom, tokens in
      success(.value(Value(atom: atom)), tokens)
    }
  }
}

struct Variable: Hashable { let name: String }
struct Value: Hashable { let atom: Atom }

typealias Substitution = [Variable: Value]

extension Array: Hashable where Element: Hashable {
  public var hashValue: Int {
    return reduce(0, { $0 ^ $1.hashValue })
  }
}

struct Atom: Parseable, Hashable {
  let predicate: String
  let subjects: [Subject]

  func unifiable(_ other: Atom) -> Bool {
    return true
  }

  static func yes(subjects: [Subject]) -> Atom {
    return Atom(predicate: "yes", subjects: subjects)
  }

  static let no = Atom(predicate: "no", subjects: [])

  static func parse<T: Collection>(tokens: T, failure: @escaping ([Exception]) -> Void, success: @escaping (Atom, T.SubSequence) -> Void) where T.Element == Token, T.Index == Int {
    String.parse(tokens: tokens, failure: { failure($0 + [Exception(description: "Atom")]) }) { predicate, tokens in
      guard predicate.lowercased() == predicate, predicate != yes(subjects: []).predicate, predicate != no.predicate else {
        failure([Exception(description: "Atom")])
        return
      }
      Punctuation.openParenthesis.parse(tokens: tokens, failure: { _ in
        success(Atom(predicate: predicate, subjects: []), tokens)
      }) { tokens in
        Group.parse(tokens: tokens, separator: .comma, failure: { failure($0 + [Exception(description: "Atom")]) }) { subjects, tokens in
          Punctuation.closeParenthesis.parse(tokens: tokens, failure: { failure($0 + [Exception(description: "Atom")]) }) { tokens in
            success(Atom(predicate: predicate, subjects: subjects), tokens)
          }
        }
      }
    }
  }
}

struct Arrow: Parseable {
  static func parse<T: Collection>(tokens: T, failure: @escaping ([Exception]) -> Void, success: @escaping (Arrow, T.SubSequence) -> Void) where T.Element == Token, T.Index == Int {
    Punctuation.less_than.parse(tokens: tokens, failure: { failure($0 + [Exception(description: "Arrow")]) }) { tokens in
      Punctuation.hyphen.parse(tokens: tokens, failure: { failure($0 + [Exception(description: "Arrow")]) }) { tokens in
        success(Arrow(), tokens)
      }
    }
  }
}

struct Body: Parseable {
  let terms: [Atom]

  static func parse<T: Collection>(tokens: T, failure: @escaping ([Exception]) -> Void, success: @escaping (Body, T.SubSequence) -> Void) where T.Element == Token, T.Index == Int {
    Group.parse(tokens: tokens, separator: .comma, failure: { failure($0 + [Exception(description: "Body")]) }) { terms, tokens in
      success(Body(terms: terms), tokens)
    }
  }
}

extension Punctuation {
  func parse<T: Collection>(tokens: T, failure: @escaping ([Exception]) -> Void, success: @escaping (T.SubSequence) -> Void) where T.Element == Token, T.Index == Int {
    guard case let .punctuation(punctuation)? = tokens.first, punctuation == self else {
      failure([Exception(description: "Punctuation: \(self)")])
      return
    }
    success(tokens.dropFirst())
  }
}

struct Sentence: Parseable {
  let head: Atom
  let body: Body

  static func parse<T: Collection>(tokens: T, failure: @escaping ([Exception]) -> Void, success: @escaping (Sentence, T.SubSequence) -> Void) where T.Element == Token, T.Index == Int {
    Atom.parse(tokens: tokens, failure: { failure($0 + [Exception(description: "Sentence")]) }) { head, tokens in
      Arrow.parse(tokens: tokens, failure: { _ in
        Punctuation.period.parse(tokens: tokens, failure: { failure($0 + [Exception(description: "Sentence")]) }) { tokens in
          success(Sentence(head: head, body: Body(terms: [])), tokens)
        }
      }) { _, tokens in
        Body.parse(tokens: tokens, failure: { failure($0 + [Exception(description: "Sentence")]) }) { body, tokens in
          Punctuation.period.parse(tokens: tokens, failure: failure) { tokens in
            success(Sentence(head: head, body: body), tokens)
          }
        }
      }
    }
  }
}

struct KnowledgeBase: Parseable {
  let sentences: [Sentence]

  static func parse<T: Collection>(tokens: T, failure: @escaping ([Exception]) -> Void, success: @escaping (KnowledgeBase, T.SubSequence) -> Void) where T.Element == Token, T.Index == Int {
    Group.parse(tokens: tokens, useAllTokens: true, failure: { failure($0 + [Exception(description: "Program")]) }) { sentences, tokens in
      success(KnowledgeBase(sentences: sentences), tokens)
    }
  }
}

extension String {
  func parse<T: Collection>(tokens: T, failure: @escaping ([Exception]) -> Void, success: @escaping (T.SubSequence) -> Void) where T.Element == Token, T.Index == Int {
    String.parse(tokens: tokens, failure: { failure($0 + [Exception(description: "String: \(self)")]) }) { text, tokens in
      guard text == self else {
        failure([Exception(description: "String: \(self)")])
        return
      }
      success(tokens)
    }
  }
}

extension String: Parseable {
  static func parse<T: Collection>(tokens: T, failure: @escaping ([Exception]) -> Void, success: @escaping (String, T.SubSequence) -> Void) where T.Element == Token, T.Index == Int {
    guard case let .string(text)? = tokens.first else {
      failure([Exception(description: "String")])
      return
    }
    success(text, tokens.dropFirst())
  }
}

struct Query: Parseable {
  let terms: [Atom]

  static func parse<T: Collection>(tokens: T, failure: @escaping ([Exception]) -> Void, success: @escaping (Query, T.SubSequence) -> Void) where T.Element == Token, T.Index == Int {
    "ask".parse(tokens: tokens, failure: { failure($0 + [Exception(description: "Query")]) }) { tokens in
      Body.parse(tokens: tokens, failure: { failure($0 + [Exception(description: "Query")]) }) { body, tokens in
        Punctuation.question.parse(tokens: tokens, failure: { failure($0 + [Exception(description: "Query")]) }) { tokens in
          success(Query(terms: body.terms), tokens)
        }
      }
    }
  }
}

struct Group {
  static func parse<T: Collection, E: Parseable>(tokens: T, separator: Punctuation? = nil, useAllTokens: Bool = false, first: Bool = true, failure: @escaping ([Exception]) -> Void, success: @escaping ([E], T.SubSequence) -> Void) where T.Element == Token, T.Index == Int {
    guard !tokens.isEmpty else {
      if first {
        failure([Exception(description: "Group")])
      } else {
        success([], tokens[...])
      }
      return
    }
    E.parse(tokens: tokens, failure: { failure($0 + [Exception(description: "Group")]) }) { e, tokens in
      if let separator = separator {
        separator.parse(tokens: tokens, failure: useAllTokens ? failure : { _ in
          success([e], tokens)
        }) { tokens in
          parse(tokens: tokens, separator: separator, useAllTokens: useAllTokens, first: false, failure: failure) { (subelements: [E], tokens: T.SubSequence) in
            var elements = [e]
            elements.append(contentsOf: subelements)
            success(elements, tokens)
          }
        }
      } else {
        parse(tokens: tokens, separator: separator, useAllTokens: useAllTokens, first: false, failure: useAllTokens ? failure : { _ in success([e], tokens)}) { (subelements: [E], tokens: T.SubSequence) in
          var elements = [e]
          elements.append(contentsOf: subelements)
          success(elements, tokens)
        }
      }
    }
  }
}

protocol Parseable {
  static func parse<T: Collection>(tokens: T, failure: @escaping ([Exception]) -> Void, success: @escaping (Self, T.SubSequence) -> Void) where T.Element == Token, T.Index == Int
}
