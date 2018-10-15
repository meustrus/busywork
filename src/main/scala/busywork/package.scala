import scala.util.Try
import scala.io.StdIn.readLine

import shapeless._

import scala.language.implicitConversions

package object busywork {

  // sealed trait Coproduct
  // sealed trait :+:[+H, +T <: Coproduct] extends Coproduct
  // case class Inl[+H, +T <: Coproduct](head: H) extends (H :+: T)
  // case class Inr[+H, +T <: Coproduct](tail: T) extends (H :+: T)
  // sealed trait CNil extends Coproduct
  // object Coproduct {
  //   def apply[C <: Coproduct] = {
  //     def apply[A](a: A)(implicit inject: Inject[A, C]): inject.Out = inject(a)
  //   }

  //   trait Inject[A, C <: Coproduct] {
  //     type Out <: C
  //     def apply(a: A): Out
  //   }
  //   object Inject {
  //     implicit def injectCaseLeft[H, T <: Coproduct]: Inject[H, H :+: T] =
  //       new Inject[H, H :+: T] {
  //         type Out = Inl[H, T]
  //         def apply(a: H): Out = Inl[H, T](a)
  //       }
  //     implicit def injectCaseRight[A, H, T <: Coproduct](
  //       implicit caseLeft: Inject[A, T]
  //     ): Inject[A, H :+: T] =
  //       new Inject[A, H :+: T] {
  //         type Out = Inr[H, caseLeft.Out]
  //         def apply(a: A): Out = Inr[H, caseLeft.Out](caseLeft(a))
  //       }
  //   }

  //   def lift[C <: Coproduct] = {
  //     def apply[A <: Coproduct](a: A)(implicit lifter: Lift[A, C]): lifter.Out = lifter(a)
  //   }

  //   trait Lifter[A <: Coproduct, C <: Coproduct] {
  //     type Out <: C
  //     def apply(a: A): Out
  //   }
  //   object Lifter {
  //     implicit def liftCaseFound[H1, T1 <: Coproduct, H2, T2 <: Coproduct, C <: H2 :+: T2](
  //       implicit next: Lifter[T1, T2]
  //     ): Lifter[Inl[H1, T1], T2] =
  //       new Lifter[Inl[H, T], C] {
  //         type Out = Inl[H, next.Out]
  //         def apply(a: Inl[H, T]): Out = Inl[H, next.Out](a.head)
  //       }

  //     implicit def liftCaseNext[H, T <: Coproduct, C <: Coproduct](
  //       implicit next: Lifter[T, C]
  //     ): Lifter[Inr[H, T], C] =
  //       new Lifter[Inr[H, T], C] {
  //         type Out = Inr[H, next.Out]
  //         def apply(a: Inr[H, T]): Out = Inr[H, next.Out](a.tail)
  //       }

  //     implicit def liftCaseEnd[C <: Coproduct]: Lifter[CNil, C] =
  //       new Lifter[CNil, C] {
  //         type Out = C
  //         def apply(a: Inr[H, T]): Out = Inr[H, next.Out](a.tail)
  //       }
  //   }

  //   def unify[C <: Coproduct](c: C)(implicit unifier: Unify): unifier.Out = unifier(c)

  //   trait Unify[C <: Coproduct] {
  //     type Out <: Coproduct
  //     def apply(c: C): Out
  //   }

  //   object Unify {
  //     trait Lub[-A, -B, Out] extends Serializable {
  //       def left(a : A): Out
  //       def right(b : B): Out
  //     }

  //     implicit def lub[T] = new Lub[T, T, T] {
  //       def left(a : T): T = a
  //       def right(b : T): T = b
  //     }

  //     implicit def unifyCaseMain[H1, H2, T <: Coproduct, L, Out0](
  //       implicit lt: Unify[H2 :+: T, L] { type Out = L },
  //       u: Lub[H1, L, Out0]
  //     ): Unify[H1 :+: H2 :+: T] =
  //       new Unify[H1 :+: H2 :+: T] {
  //         type Out = Out0
  //         def apply(c: H1 :+: H2 :+: T): Out = c match {
  //           case Inl(h1) => u.left(h1)
  //           case Inr(t) => u.right(lt(t))
  //         }
  //       }

  //     implicit def unifyCaseLast[H]: Aux[H :+: CNil, H] =
  //       new Unify[H :+: CNil] {
  //         type Out = H
  //         def apply(c: H :+: CNil): Out = (c: @unchecked) match {
  //           case Inl(h) => h
  //         }
  //       }
  //   }
  // }

  type |:[+A, +B] = Either[A, B]

  trait Work {
    type Result
  }
  object Work {
    type Aux[R] = Work { type Result = R }
  }

  trait Executor[W <: Work] extends (W => W#Result)

  class Executors[L <: HList](underlying: L) extends Poly1 {
    import shapeless.ops.hlist._
    import shapeless.poly._
    implicit def caseSelectable[W <: Work](implicit selector: Selector[L, W => W#Result]) =
      Case1[this.type, W, W => W#Result](selector(underlying))
  }

  // sealed with typeclass?
  trait Program[C <: Coproduct] { self =>
    import shapeless.ops.coproduct._
    type Result
    def map[R, M <: Work.Aux[R]](fa: Result => M)(implicit prepend: Prepend[C, M :+: CNil]): Program[prepend.Out] =
      Program.FlatMap[
        C,
        M :+: CNil,
        prepend.Out,
        self.Result,
        R
      ](this, (x: Result) => Program.Pure(fa(x)))(prepend)
    def flatMap[R, M <: Coproduct](fa: Result => Program.Aux[M, R])(implicit prepend: Prepend[C, M]): Program[prepend.Out] =
      Program.FlatMap[
        C,
        M,
        prepend.Out,
        self.Result,
        R
      ](this, fa)(prepend)
    def run(executor: C => Result)
  }
  type Program1[A] = Program[A :+: CNil]
  type Program2[A, B] = Program[A :+: B :+: CNil]
  type Program3[A, B, C] = Program[A :+: B :+: C :+: CNil]
  type Program4[A, B, C, D] = Program[A :+: B :+: C :+: D :+: CNil]
  type Program5[A, B, C, D, E] = Program[A :+: B :+: C :+: D :+: E :+: CNil]
  type Program6[A, B, C, D, E, F] = Program[A :+: B :+: C :+: D :+: E :+: F :+: CNil]
  type Program7[A, B, C, D, E, F, G] = Program[A :+: B :+: C :+: D :+: E :+: F :+: G :+: CNil]
  type Program8[A, B, C, D, E, F, G, H] = Program[A :+: B :+: C :+: D :+: E :+: F :+: G :+: H :+: CNil]
  type Program9[A, B, C, D, E, F, G, H, I] = Program[A :+: B :+: C :+: D :+: E :+: F :+: G :+: H :+: I :+: CNil]
  type Program10[A, B, C, D, E, F, G, H, I, J] =
    Program[A :+: B :+: C :+: D :+: E :+: F :+: G :+: H :+: I :+: J :+: CNil]
  type Program11[A, B, C, D, E, F, G, H, I, J, K] =
    Program[A :+: B :+: C :+: D :+: E :+: F :+: G :+: H :+: I :+: J :+: K :+: CNil]
  type Program12[A, B, C, D, E, F, G, H, I, J, K, L] =
    Program[A :+: B :+: C :+: D :+: E :+: F :+: G :+: H :+: I :+: J :+: K :+: L :+: CNil]
  type Program13[A, B, C, D, E, F, G, H, I, J, K, L, M] =
    Program[A :+: B :+: C :+: D :+: E :+: F :+: G :+: H :+: I :+: J :+: K :+: L :+: M :+: CNil]
  type Program14[A, B, C, D, E, F, G, H, I, J, K, L, M, N] =
    Program[A :+: B :+: C :+: D :+: E :+: F :+: G :+: H :+: I :+: J :+: K :+: L :+: M :+: N :+: CNil]
  type Program15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] =
    Program[A :+: B :+: C :+: D :+: E :+: F :+: G :+: H :+: I :+: J :+: K :+: L :+: M :+: N :+: O :+: CNil]
  type Program16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] =
    Program[A :+: B :+: C :+: D :+: E :+: F :+: G :+: H :+: I :+: J :+: K :+: L :+: M :+: N :+: O :+: P :+: CNil]
  type Program17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] =
    Program[A :+: B :+: C :+: D :+: E :+: F :+: G :+: H :+: I :+: J :+: K :+: L :+: M :+: N :+: O :+: P :+: Q :+: CNil]
  type Program18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] =
    Program[A :+: B :+: C :+: D :+: E :+: F :+: G :+: H :+: I :+: J :+: K :+: L :+: M :+: N :+: O :+: P :+: Q :+: R :+: CNil]
  type Program19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] =
    Program[A :+: B :+: C :+: D :+: E :+: F :+: G :+: H :+: I :+: J :+: K :+: L :+: M :+: N :+: O :+: P :+: Q :+: R :+: S :+: CNil]
  type Program20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] =
    Program[A :+: B :+: C :+: D :+: E :+: F :+: G :+: H :+: I :+: J :+: K :+: L :+: M :+: N :+: O :+: P :+: Q :+: R :+: S :+: T :+: CNil]
  type Program21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] =
    Program[A :+: B :+: C :+: D :+: E :+: F :+: G :+: H :+: I :+: J :+: K :+: L :+: M :+: N :+: O :+: P :+: Q :+: R :+: S :+: T :+: U :+: CNil]
  type Program22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] =
    Program[A :+: B :+: C :+: D :+: E :+: F :+: G :+: H :+: I :+: J :+: K :+: L :+: M :+: N :+: O :+: P :+: Q :+: R :+: S :+: T :+: U :+: V :+: CNil]
  object Program {
    import shapeless.ops.coproduct._

    type Aux[C <: Coproduct, R] = Program[C] { type Result = R }

    case class Pure[A <: Work](work: A) extends Program[A :+: CNil] { self =>
      type Result = work.Result
      def run(executor: (A :+: CNil) => Result): Result = executor(Coproduct[A :+: CNil](work))
    }
    case class FlatMap[
      AC <: Coproduct,
      BC <: Coproduct,
      C <: Coproduct,
      AR,
      BR
    ](first: Aux[AC, AR], mapper: AR => Aux[BC, BR])(implicit prepend: Prepend.Aux[AC, BC, C]) extends Program[C] {
      type Result = BR
      // to make this work, `executor` needs to return Work#Result depending on the actual type
      // which can't happen with this design because `executor` would try to pick its Result at runtime
      // (use `executors`, an hlist of (Work => Work#Result) containing everything in C?)
      def run[L <: HList](executors: Executors[L])(implicit mapper: Mapper[Executors[L], C]) = {
        first.run(executors)
        val ar: AR = first.run((ac: AC) => (prepend(Left(ac)).map(executors))())
        mapper(ar).run((bc: BC) => executor(prepend(Right(bc))))
      }
    }
    // case class Aligned[AC <: Coproduct, C <: Coproduct](ac: AC)()

    // trait



    // use `align` to tell whether two programs have the same types of work
    // def align[K <: Coproduct](implicit align: Align[C, K]): K = align(c)
    // create a `flatten` function; use it to compare programs with combined

    def apply[A <: Work](chore: A): Program1[A] = Pure(chore)

    implicit class ProgramOps[C <: Coproduct, R, A <: Aux[C, R]](work: A) {
      // def map[B](f: R => B) =
      //   FlatMap(chore, (a: R) => Pure(f(a)))
      // def flatMap[C2 <: Coproduct, R2, B <: Aux[C2, R2]](f: R => B)(
      //   implicit 
      // ): Program[A :+: B :+: CNil] =
      //   FlatMap[C, C2, R, R2, A, B](work, f)
    }
  }

  sealed trait GameWork[R] extends Work { type Result = R }
  object GameWork {
    sealed trait Output extends GameWork[Unit]
    object Output {
      case class YouGuessedRight(name: String) extends Output
      implicit class YouGuessedRightPrintable(a: YouGuessedRight) extends Printable[YouGuessedRight] {
        def en = s"You guessed right, ${a.name}!"
      }
      case class YouGuessedWrong(name: String, num: Int) extends Output
      implicit class YouGuessedWrongPrintable(a: YouGuessedWrong) extends Printable[YouGuessedWrong] {
        def en = s"You guessed wrong, ${a.name}! The number was: ${a.num}"
      }
      case class DoYouWantToContinue(name: String) extends Output
      implicit class DoYouWantToContinuePrintable(a: DoYouWantToContinue) extends Printable[DoYouWantToContinue] {
        def en = s"Do you want to continue, ${a.name}?"
      }
      case class PleaseGuess(name: String) extends Output
      implicit class PleaseGuessPrintable(a: PleaseGuess) extends Printable[PleaseGuess] {
        def en = s"Dear ${a.name}, please guess a number from 1 to 5:"
      }
      case class ThatIsNotValid(name: String) extends Output
      implicit class ThatIsNotValidPrintable(a: ThatIsNotValid) extends Printable[ThatIsNotValid] {
        def en = s"That is not a valid selection, ${a.name}!"
      }
      case object WhatIsYourName extends Output
      implicit class WhatIsYourNamePrintable(a: WhatIsYourName.type) extends Printable[WhatIsYourName.type] {
        def en = "What is your name?"
      }
      case class WelcomeToGame(name: String) extends Output
      implicit class WelcomeToGamePrintable(a: WelcomeToGame) extends Printable[WelcomeToGame] {
        def en = s"Hello, ${a.name}, welcome to the game!"
      }
    }
    trait Printable[A] {
      def en: String
    }
    case object Input extends GameWork[String]
  }

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def checkAnswer(name: String, num: Int, guess: Int): GameWork.Output =
    if (num == guess) GameWork.Output.YouGuessedRight(name)
    else GameWork.Output.YouGuessedWrong(name, num)

  // def checkContinue(name: String): Program2[Work.Pure, GameWork] =
  //   for {
  //     _      <- GameWork.Output.DoYouWantToContinue(name)
  //     choice <- GameWork.Input
  //     cont   <- choice.toLowerCase match {
  //       case "y" => Work.Pure(true)
  //       case "n" => Work.Pure(false)
  //       case _   => checkContinue(name)
  //     }
  //   } yield cont

  // def gameLoop[F[_]: Program: Console: Random](name: String): F[Unit] =
  //   for {
  //     num   <- nextInt(5).map(_ + 1)
  //     _     <- putStrLn(ConsoleOut.PleaseGuess(name))
  //     guess <- getStrLn
  //     _     <- parseInt(guess).fold(
  //                putStrLn(ConsoleOut.ThatIsNotValid(name))
  //              )((guess: Int) => checkAnswer(name, num, guess))
  //     cont  <- checkContinue(name)
  //     _     <- if (cont) gameLoop(name) else finish(())
  //   } yield ()

  // def main[F[_]: Program: Console: Random]: F[Unit] =
  //   for {
  //     _     <- putStrLn(ConsoleOut.WhatIsYourName)
  //     name  <- getStrLn
  //     _     <- putStrLn(ConsoleOut.WelcomeToGame(name))
  //     _     <- gameLoop(name)
  //   } yield ()
}
