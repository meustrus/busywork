

// package busywork

// sealed trait BU[+A] { type Upper = A }
// sealed trait :^[+T <: BU[_], +H <: T#Upper] extends BU[T#Upper]
// sealed trait ^^[+T <: BU[_], +H <: T#Upper] extends BU[T#Upper]
// case class Binl[+T <: BU[_], +H <: T#Upper](tail: T) extends ^^[T, H]
// case class Binr[+T <: BU[_], +H <: T#Upper](head: H) extends ^^[T, H]

// object BU {
//   sealed trait Of[+A] extends BU[A]
//   sealed trait Real[+A] extends BU[A]
//   // type Aux[A] = BU { type Upper = A }
// }

// object usage {
//   trait Alpha
//   case object A extends Alpha
//   case object B extends Alpha
//   case object C extends Alpha

//   type AAA = BU.Of[A.type] :^ A.type
//   type AA = BU.Of[Alpha] :^ A.type
//   type AB = BU.Of[Alpha] :^ A.type ^^ B.type

//   type AlphaUnion = BU.Of[Alpha]
//   type UABC = AlphaUnion :^ A.type ^^ B.type ^^ C.type
// }
