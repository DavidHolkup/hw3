package hw3
import scala.math.sqrt

object Main {
  def standardDeviation(vector: List[Double]): Double = {
    val n = vector.length
    val xn = vector.sum / n

    val sn2 = vector.map(x => {
      (x - xn) * (x - xn)
    }).sum / n
    sqrt(sn2)
  }

  def letterFrequencyRanking(corpus: String): String = {
    val forbidden = ' ' :: '\n' :: '\t' :: ':' :: ';' ::
                    '\\' :: '/' :: '|' ::
                    '-' :: '_' :: '‐' :: '…' :: '»' :: '«' ::
                    '.' :: ',' :: '!' :: '¡' :: '?' ::'¿' ::
                    '(' :: ')' :: '[' :: ']' :: '{' :: '}' ::
                    '\'' :: '"' :: '`' :: '„' :: '‚' :: Nil

    val letters = List.from(
      corpus.toLowerCase.foldLeft(Map.empty[Char, Letter]) {
      (map, char) => {
        if (map.contains(char)) {
          map + (char -> map(char).++)
        } else if (forbidden.contains(char)) {
          map
        } else {
          map + (char -> new Letter(char))
        }
      }
    }.values)

    letters.sortWith(Letter.compare)
      .map(_.toString).foldLeft("")(_+_)
  }

  def romanji(katakana: String): String = ???

  def gray(bits: Int): List[String] = {
    def inner(n: Int, first: List[String]): List[String] = {
      if (n == 1) {
        return first
      }

      val next: List[String] = first.map(x => s"0$x") ::: first.reverse.map(x => s"1$x")
      inner(n -1, next)
    }
    if (bits == 0) return List.empty[String]
    else if (bits < 0) throw new IllegalArgumentException
    inner(bits, "0" :: "1" :: Nil)
  }
}

object Katakana {
  val symbols = Map(
    'ア' -> List('a'), 'イ' -> List('i'),  'ウ' -> List('u'), 'エ' -> List('e'), 'オ' -> List('o'),
    'ン' -> List('n'),
    'カ' -> List('k','a'), 'キ' -> List('k','i'), 'ク' -> List('k','u'), 'ケ' -> List('k','e'), 'コ' -> List('k','o'),
    'ガ' -> List('g','a'), 'ギ' -> List('g','i'), 'グ' -> List('g','u'), 'ゲ' -> List('g','e'), 'ゴ' -> List('g','o'),
    'サ' -> List('s','a'), 'シ' -> List('s','i'), 'ス' -> List('s','u'), 'セ' -> List('s','e'), 'ソ' -> List('s','o'),
    'ザ' -> List('z','a'), 'ジ' -> List('z','i'), 'ズ' -> List('z','u'), 'ゼ' -> List('z','e'), 'ゾ' -> List('z','o'),
    'タ' -> List('t','a'), 'チ' -> List('t','i'), 'ツ' -> List('t','u'), 'テ' -> List('t','e'), 'ト' -> List('t','o'),
    'ダ' -> List('d','a'), 'ヂ' -> List('d','i'), 'ヅ' -> List('d','u'), 'デ' -> List('d','e'), 'ド' -> List('d','o'),
    'ナ' -> List('n','a'), 'ニ' -> List('n','i'), 'ヌ' -> List('n','u'), 'ネ' -> List('n','e'), 'ノ' -> List('n','o'),
    'ハ' -> List('h','a'), 'ヒ' -> List('h','i'), 'フ' -> List('h','u'), 'ヘ' -> List('h','e'), 'ホ' -> List('h','o'),
    'バ' -> List('b','a'), 'ビ' -> List('b','i'), 'ブ' -> List('b','u'), 'ベ' -> List('b','e'), 'ボ' -> List('b','o'),
    'パ' -> List('p','a'), 'ピ' -> List('p','i'), 'プ' -> List('p','u'), 'ペ' -> List('p','e'), 'ポ' -> List('p','o'),
    'マ' -> List('m','a'), 'ミ' -> List('m','i'), 'ム' -> List('m','u'), 'メ' -> List('m','e'), 'モ' -> List('m','o'),
    'ヤ' -> List('y','a'),                        'ユ' -> List('y','u'),                        'ヨ' -> List('y','o'),
    'ラ' -> List('r','a'), 'リ' -> List('r','i'), 'ル' -> List('r','u'), 'レ' -> List('r','e'), 'ロ' -> List('r','o'),
    'ワ' -> List('w','a'), 'ヰ' -> List('w','i'),                        'ヱ' -> List('w','e'), 'ヲ' -> List('w','o'),
  )
  val longVowels = Map(
    'a' -> 'ā',
    'i' -> 'ī',
    'e' -> 'ē',
    'u' -> 'ū',
    'o' -> 'ō'
  )
}


class Letter (val char: Char, val count: Int = 1) {
  def < (other: Letter): Boolean = {
    if (count < other.count) return true
    if (count > other.count) return false
    char > other.char
  }

  def equals(o: Letter): Boolean = {
    char == o.char
  }

  def + (int: Int): Letter = {
    new Letter(char, count + int)
  }

  def ++ (): Letter = this + 1

  override def toString: String = char.toString

  override def hashCode(): Int = char.hashCode()
}

object Letter {
  def compare (a: Letter, b: Letter) = b < a
}
