package Board

object Color {
  sealed trait ColorVal
  case object Black extends ColorVal
  case object White extends ColorVal
  val Color = Seq(Black,White)
}
