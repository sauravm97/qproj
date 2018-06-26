let whitespace = " \t\n"
let commentPrefix: Character = "%"

enum Punctuation: Character {
  case less_than = "<"
  case hyphen = "-"
  case comma = ","
  case period = "."
  case question = "?"
  case openParenthesis = "("
  case closeParenthesis = ")"

  static var allValues: [Punctuation] {
    return [.less_than, .hyphen, .comma, .period, .question, .openParenthesis, .closeParenthesis]
  }
}

enum Token {
  case punctuation(p: Punctuation)
  case string(text: String)
}

func tokenise(code: String) -> [Token] {
  var chars: [Character] = []
  var tokens: [Token] = []
  let registerLiteral = {
    if !chars.isEmpty {
      tokens.append(.string(text: String(chars)))
      chars = []
    }
  }
  var isComment = false
  for char in code {
    if isComment {
      if char == "\n" {
        isComment = false
      } else {
        continue
      }
    }
    if char == commentPrefix {
      isComment = true
      continue
    }
    guard !whitespace.contains(char) else {
      registerLiteral()
      continue
    }
    guard !(Punctuation.allValues.map { $0.rawValue }.contains(char)) else {
      registerLiteral()
      tokens.append(.punctuation(p: Punctuation(rawValue: char)!))
      continue
    }
    chars.append(char)
  }
  registerLiteral()
  return tokens
}
