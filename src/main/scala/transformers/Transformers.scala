package transformers

object Transformers {
  private type Transformer = String => String

  def transform: String => Transformer => String = s => _(s)

  def duplicate: Transformer = _ * 2
  def truncateHalf: Transformer = s => s.substring(0, s.length / 2)
  def reverse: Transformer = _.reverse
}
