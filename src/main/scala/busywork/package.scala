import scala.util.Try
import scala.io.StdIn.readLine

import shapeless._
import shapeless.ops._
import shapeless.poly._

import scala.language.implicitConversions

package object busywork {

  trait Work {
    type Result
  }
  object Work {
    type Aux[R] = Work { type Result = R }
  }

  trait Executor[WorkType <: Work] extends (WorkType => WorkType#Result)

  trait FindExecutors[WorkTypeUnion <: Coproduct] {
    type Out <: HList
    def apply: Out
    // can prove this much
    // def apply[WorkType <: Work](
    //   implicit selector: coproduct.Selector[WorkTypeUnion, WorkType]
    // ): Out
  }
  object FindExecutors {
    implicit def caseCons[WorkType <: Work, T <: Coproduct](
      implicit tail: FindExecutors[T],
      executor: Executor[WorkType]
    ): FindExecutors[WorkType :+: T] = new FindExecutors[WorkType :+: T] {
      type Out = Executor[WorkType] :: tail.Out
      def apply: Out = executor :: tail.apply
    }

    implicit def caseNil: FindExecutors[CNil] = new FindExecutors[CNil] {
      type Out = HNil
      def apply: Out = HNil
    }
  }

  trait Execute[WorkTypeUnion <: Coproduct] extends Poly1

  // ExecutorTypeList bounded to (WorkType <: Work) => (WorkType#Result)
  // Given [WorkTypeUnion <: Coproduct, ExecutorTypeList <: HList]
  // Execute is a poly function
  //   from [WorkType <: Work : coproduct.Selector[WorkTypeUnion, ?]]
  //   to   [WorkType#Result]
  // In order to extract this result, there must exist:
  //   hlist.Selector[ExecutorTypeList, Executor[WorkType]]
  class BuildExecutors[WorkType <: Work, CT <: Coproduct, LT <: HList](
    executors: Executor[WorkType] :: LT
  ) extends Poly1 with Execute[WorkType :+: CT] {
    implicit def caseHead = Case1[this.type, WorkType, WorkType#Result](executors.head)
    // implicit def caseTail[W <: Work] = Case1[this.type, 
    // implicit def chained[W <: Work, CTT <: Coproduct, LTT <: HList]: BuildExecutors[W, CTT, LTT] =
    //   new BuildExecutors
    // implicit def caseSelectable[W <: Work](implicit selector: hlist.Selector[Executor[WorkType] :: LT, W => W#Result]) =
    //   Case1[this.type, W, W#Result](selector(executors)(_))
  }

  // sealed with typeclass?
  trait Program[+WorkTypeUnion <: Coproduct] {
    type Result

    def map[R, M <: Work.Aux[R], NewWorkTypeUnion <: Coproduct](fa: Result => M)(
      implicit initEmbedBasis: coproduct.Basis[NewWorkTypeUnion, WorkTypeUnion],
      mappedEmbedBasis: coproduct.Basis[NewWorkTypeUnion, M :+: CNil],
      prepend: coproduct.Prepend.Aux[WorkTypeUnion, M :+: CNil, NewWorkTypeUnion]
      // implicit prepend: coproduct.Prepend[WorkTypeUnion, M :+: CNil]
    ): Program[prepend.Out] =
      Program.FlatMap[WorkTypeUnion, M :+: CNil, prepend.Out, this.type, Program.Pure[M]](
      // Program.FlatMap[WorkTypeUnion, M :+: CNil, prepend.Out, Result, R](
        this,
        (x: Result) => Program.Pure(fa(x))
      )
    def flatMap[M <: Coproduct, P <: Program[M], C <: Coproduct](fa: Result => P)(
      implicit initEmbedBasis: coproduct.Basis[C, WorkTypeUnion],
      mappedEmbedBasis: coproduct.Basis[C, M],
      prepend: coproduct.Prepend.Aux[WorkTypeUnion, M, C]
    // def flatMap[R, M <: Coproduct](fa: Result => Program.Aux[M, R])(
      // implicit prepend: coproduct.Prepend[WorkTypeUnion, M]
    ): Program[prepend.Out] =
      Program.FlatMap[WorkTypeUnion, M, prepend.Out, this.type, P](this, fa)
      // Program.FlatMap[WorkTypeUnion, M, prepend.Out, Result, R](this, fa)(prepend)
    // def run(executor: WorkTypeUnion => Result)
    // def run[C <: Coproduct, L <: HList](execute: Execute[L])(
    //   implicit m: coproduct.Mapper[execute.type, WorkTypeUnion],
    //   embedBasis: coproduct.Basis[C, WorkTypeUnion]
    // ): m.Out
    def run[ExecutorList <: HList](execute: Execute[ExecutorList])(
      implicit mapper: ConcreteMapper.Aux[execute.type, WorkTypeUnion, Result]
    ): Result
  }

  object Program {

    type Aux[WorkTypeUnion <: Coproduct, R] = Program[WorkTypeUnion] { type Result = R }

    def apply[A <: Work](work: A): Program1[A] = Pure(work)

    case class Pure[WorkType <: Work](work: WorkType) extends Program[Inl[WorkType, CNil]] {
      type Result = work.Result
      def run[ExecutorList <: HList](execute: Execute[ExecutorList])(
        implicit mapper: ConcreteMapper.Aux[execute.type, Inl[WorkType, CNil], Result]
      ): Result = mapper[Inl[WorkType, CNil]](Inl[WorkType, CNil](work))
    }
    case class FlatMap[
      InitWorkTypeUnion <: Coproduct,
      MappedWorkTypeUnion <: Coproduct,
      WorkTypeUnion <: Coproduct,
      InitProgram <: Program[InitWorkTypeUnion],
      MappedProgram <: Program[MappedWorkTypeUnion]
    ](
      init: InitProgram,
      mapFn: InitProgram#Result => Program[MappedWorkTypeUnion]
    )(
      implicit initEmbedBasis: coproduct.Basis[WorkTypeUnion, InitWorkTypeUnion],
      mappedEmbedBasis: coproduct.Basis[WorkTypeUnion, MappedWorkTypeUnion]
    ) extends Program[WorkTypeUnion] {
      type Result = MappedProgram#Result

      // logic:
      // Because I know I can map any W in A <: Coproduct
      //   to a W => W#Result in ExecutorList <: HList,
      //   : Mapper
      // I know I can map any W to a W => W#Result with ExecutorList <: HList
      def run[ExecutorList <: HList](execute: Execute[ExecutorList])(
        implicit mapper: ConcreteMapper.Aux[execute.type, WorkTypeUnion, Result]
      ): Result = {
        implicit def subMapper[Sub <: Coproduct](
          implicit basis: coproduct.Basis[WorkTypeUnion, Sub]
        ): coproduct.Mapper[execute.type, Sub] = new coproduct.Mapper[execute.type, Sub] {
          type Out = mapper.Out
          def apply(sub: Sub): Out = mapper(basis.inverse(Right(sub)))
        }
        mapFn(init.run(execute)).run(execute)
      }
    }
    // case class Aligned[AC <: Coproduct, C <: Coproduct](ac: AC)()

    // trait



    // use `adjoined` function to compare programs with combined
    // def adjoined(implicit adjoin: Adjoin[C]): adjoin.Out = adjoin(c)
    // use `unify` and `align` functions to 
    // use `align` to tell whether two programs have the same types of work
    // def align[K <: Coproduct](implicit align: Align[C, K]): K = align(c)

    implicit class ProgramOps[C <: Coproduct, R, A <: Aux[C, R]](work: A) {
      // def map[B](f: R => B) =
      //   FlatMap(chore, (a: R) => Pure(f(a)))
      // def flatMap[C2 <: Coproduct, R2, B <: Aux[C2, R2]](f: R => B)(
      //   implicit 
      // ): Program[A :+: B :+: CNil] =
      //   FlatMap[C, C2, R, R2, A, B](work, f)
    }
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
