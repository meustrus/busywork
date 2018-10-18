package busywork

import scala.util.Try
import scala.io.StdIn.readLine

import shapeless._
import shapeless.ops._
import shapeless.poly._

import scala.language.implicitConversions

object Graveyard {

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


  // trait CoproductToHList[C <: Coproduct, L <: HList] { type Out <: Coproduct }

  // object CoproductToHList {
  //   def apply[C <: Coproduct, L <: HList](implicit mapper: CoproductToHList[C, L]): Aux[C, L, mapper.Out] = mapper

  //   type Aux[C <: Coproduct, L <: HList, Out0 <: Coproduct] = CoproductToHList[C, L] { type Out = Out0 }

  //   implicit def cnilMapper: Aux[CNil, HNil, CNil] = new CoproductToHList[CNil, HNil] {
  //     type Out = CNil
  //     def apply(t: CNil): Out = t
  //   }

  //   implicit def cpMapper[H, CT <: Coproduct, LT <: HList](
  //     implicit chained: CoproductToHList[CT, LT]
  //   ): Aux[H :+: CT, H :: LT, H :+: chained.Out] = new CoproductToHList[H :+: CT, H :: LT] {
  //     type Out = H :+: chained.Out
  //     def apply(c: H :+: CT): Out = 
  //   }

    // implicit def cpMapper[F <: Poly, H, OutH, T <: Coproduct]
    //   (implicit fh: Case1.Aux[F, H, OutH], mt: Mapper[F, T]): Aux[F, H :+: T, OutH :+: mt.Out] =
    //     new Mapper[F, H :+: T] {
    //       type Out = OutH :+: mt.Out
    //       def apply(c: H :+: T): Out = c match {
    //         case Inl(h) => Inl(fh(h))
    //         case Inr(t) => Inr(mt(t))
    //       }
    //     }
  // }


  trait Extract[C <: :+:[_, _]] {
    type Out
    def apply(coproduct: C): Out
  }

  object Extract {
    type Aux[C <: :+:[_, _], Out0] = Extract[C] { type Out = Out0 }

    // should only fail when type is not concrete
    def apply[C <: :+:[_, _]](implicit extract: Extract[C]): Extract[C] = extract

    implicit def caseInl[H, T <: :+:[_, _]]: Extract.Aux[Inl[H, T], H] =
      new Extract[Inl[H, T]] {
        type Out = H
        def apply(coproduct: Inl[H, T]): Out = coproduct.head
      }

    implicit def caseInr[H, T <: :+:[_, _]](
      implicit extractTail: Extract[T]
    ): Extract.Aux[Inr[H, T], extractTail.Out] =
      new Extract[Inr[H, T]] {
        type Out = extractTail.Out
        def apply(coproduct: Inr[H, T]): Out = extractTail(coproduct.tail)
      }
  }

  trait ConcreteMapper[F <: Poly, C <: Coproduct] {
    type In
    type Out
    def apply[RealCoproduct <: C](c: RealCoproduct)(
      implicit extract: Extract.Aux[RealCoproduct, In]
    ): Out
  }

  object ConcreteMapper {
    type Aux[F <: Poly, C <: Coproduct, Out0] = ConcreteMapper[F, C] { type Out = Out0 }

    implicit def cnilMapper[F <: Poly]: Aux[F, CNil, CNil] = new ConcreteMapper[F, CNil] {
      type In = Nothing
      type Out = CNil
      def apply[RealCoproduct <: CNil](c: RealCoproduct)(
        implicit extract: Extract.Aux[RealCoproduct, In]
      ): Out = c
    }

    implicit def cpMapper[F <: Poly, H, OutH, T <: Coproduct](
      implicit fh: Case1.Aux[F, H, OutH],
      mt: ConcreteMapper[F, T]
    ): Aux[F, H :+: T, OutH] =
      new ConcreteMapper[F, H :+: T] {
        type In = H
        type Out = OutH
        def apply[RealCoproduct <: H :+: T](c: RealCoproduct)(
          implicit extract: Extract.Aux[RealCoproduct, In]
        ): OutH = fh(extract(c))
      }

    // implicit def cpMapper[F <: Poly, H, OutH, T <: Coproduct]
    //   (implicit fh: Case1.Aux[F, H, OutH], mt: ConcreteMapper[F, T]): Aux[F, H :+: T, OutH :+: mt.Out] =
    //     new ConcreteMapper[F, H :+: T] {
    //       type Out = OutH :+: mt.Out
    //       def apply(c: H :+: T): Out = c match {
    //         case Inl(h) => Inl(fh(h))
    //         case Inr(t) => Inr(mt(t))
    //       }
    //       def apply[C <: H :+: T](c: C): 
    //     }

    // implicit def inlMapper[F <: Poly, H, OutH, T <: Coproduct](
    //   implicit fh: Case1.Aux[F, H, OutH], mt: ConcreteMapper[F, T]
    // ): Aux[F, Inl[H, T], OutH] = new Mapper[F, Inl[H, T]] {
    //   type Out = OutH
    //   def apply(c: Inl[H, T]): Out = fh(c.head)
    // }

    // implicit def inrMapper[F <: Poly, H, OutH, T <: Coproduct](
    //   implicit fh: Case1.Aux[F, H, OutH], mt: Mapper[F, T]
    // ): Aux[F, Inr[H, T], mt.Out] = new Mapper[F, Inr[H, T]] {
    //   type Out = mt.Out
    //   def apply(c: Inr[H, T]): Out = mt(c.tail)
    // }
  }

  // type |:[+A, +B] = Either[A, B]

  // trait SelectorChain[L <: HList, C <: Coproduct] {
  //   // type SelectorHList <: HList
  //   // def selectors: SelectorHList
  //   def select[RealCoproduct <: C](l: L, c: RealCoproduct)(
  //     implicit extract: Extract.Aux[C, RealCoproduct]
  //   ): extract.Out
  // }
  // object SelectorChain {
  //   type Aux[L <: HList, C <: Coproduct, SelectorHList0 <: HList] =
  //     SelectorChain[L, C] { type SelectorHList = SelectorHList0 }

  //   implicit def caseNil[L <: HList]: SelectorChain[L, CNil] = new SelectorChain[L, CNil] {
  //     type SelectorHList = HNil
  //     val selectors: SelectorHList = HNil
  //   }

  //   implicit def caseMain[L <: HList, CH, CT <: Coproduct](
  //     implicit more: SelectorChain[L, CT],
  //     selector: hlist.Selector[L, CH]
  //   ): SelectorChain[L, CH :+: CT] = new SelectorChain[L, CH :+: CT] {
  //     // type SelectorHList = hlist.Selector[L, CH] :: more.SelectorHList
  //     // val selectors: SelectorHList = selector :: more.selectors
  //     def select[RealCoproduct <: CH :+: CT](l: L, c: RealCoproduct)(
  //       implicit extract: Extract.Aux[C, RealCoproduct]
  //     ): extract.Out = ???
  //   }
  // }

  // trait HContains[L <: HList, C <: Coproduct]
  // object HContains {
  //   type Aux[L <: HList, C <: Coproduct, SelectorHList0 <: HList] =
  //     HContains[L, C]// { type SelectorHList = SelectorHList0 }

  //   implicit def caseNil[L <: HList]: HContains[L, CNil] = new HContains[L, CNil] {
  //     // type SelectorHList = HNil
  //     // val selectors: SelectorHList = HNil
  //   }

  //   implicit def caseMain[L <: HList, CH, CT <: Coproduct](
  //     implicit more: HContains[L, CT],
  //     selector: hlist.Selector[L, CH]
  //   ): HContains[L, CH :+: CT] = new HContains[L, CH :+: CT] {
  //     // type SelectorHList = hlist.Selector[L, CH] :: more.SelectorHList
  //     // val selectors: SelectorHList = selector :: more.selectors
  //     def extract[A](implicit extractor: Extract.Aux[CH :+: CT, A]): A = extractor
  //   }
  // }

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
      // def run[C <: Coproduct, L <: HList](execute: Execute[L])(
      //   implicit m: coproduct.Mapper[execute.type, C],
      //   embedBasis: coproduct.Basis[C, WorkType :+: CNil]
      // ): m.Out = m(Coproduct[WorkType :+: CNil](work).embed[C])
    }
    case class FlatMap[
      InitWorkTypeUnion <: Coproduct,
      MappedWorkTypeUnion <: Coproduct,
      WorkTypeUnion <: Coproduct,
      InitProgram <: Program[InitWorkTypeUnion],
      MappedProgram <: Program[MappedWorkTypeUnion]
      // InitResult,
      // MappedResult
    ](
      init: InitProgram,
      mapFn: InitProgram#Result => Program[MappedWorkTypeUnion]
      // init: Aux[InitWorkTypeUnion, InitResult],
      // mapFn: InitResult => Aux[MappedWorkTypeUnion, MappedResult]
    )(
      implicit initEmbedBasis: coproduct.Basis[WorkTypeUnion, InitWorkTypeUnion],
      mappedEmbedBasis: coproduct.Basis[WorkTypeUnion, MappedWorkTypeUnion]
    ) extends Program[WorkTypeUnion] {
      type Result = MappedProgram#Result
      // type Result = MappedResult

      // logic:
      // Because I know I can map any W in A <: Coproduct
      //   to a W => W#Result in ExecutorList <: HList,
      //   : Mapper
      // I know I can map any W to a W => W#Result with ExecutorList <: HList

      // ConcreteMapper won't work here because we don't know at this level what
      // the actual Coproduct type will be
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
      // def run[C <: Coproduct, L <: HList](executors: Executors[L])(
      //   implicit m: coproduct.Mapper[executors.type, C],
      //   embedBasis: coproduct.Basis[C, WorkTypeUnion]
      // ): m.Out = {
      //   // implicit def chainedMapper[Sub <: Coproduct](
      //   //   implicit mapperEmbedBasis: coproduct.Basis[WorkTypeUnion, Sub]
      //   // ): coproduct.Mapper.Aux[executors.type, Sub, m.Out] = new coproduct.Mapper[executors.type, Sub] {
      //   //   type Out = m.Out
      //   //   def apply(sub: Sub): Out = m(mapperEmbedBasis.inverse(Right(sub)))
      //   // }
      //   implicit def combinedEmbedBasis[
      //     Mid <: Coproduct,
      //     Sub <: Coproduct,
      //     MidRest <: Coproduct,
      //     SubRest <: Coproduct,
      //     Rest0 <: Coproduct
      //   ](
      //     implicit midSuperSub: coproduct.Basis.Aux[Mid, Sub, SubRest],
      //     cSuperMid: coproduct.Basis.Aux[C, Mid, MidRest],
      //     restSuperSubRest: coproduct.Basis.Aux[Rest0, SubRest, MidRest],
      //     prepend: coproduct.Prepend.Aux[MidRest, SubRest, Rest0]
      //   ): coproduct.Basis.Aux[C, Sub, Rest0] = new coproduct.Basis[C, Sub] {
      //     type Rest = Rest0
      //     def apply(s: C): Either[Rest, Sub] =
      //       cSuperMid(s).fold(
      //         (midRest: MidRest) => Left(prepend(Left(midRest))),
      //         (mid: Mid)         => midSuperSub(mid).left.map { (subRest: SubRest) =>
      //           prepend(Right(subRest))
      //         }
      //       )
      //     def inverse(e: Either[Rest, Sub]): C =
      //       e.fold(
      //         (rest: Rest) => cSuperMid.inverse(
      //           restSuperSubRest(rest)
      //             .right.map((subRest: SubRest) => midSuperSub.inverse(Left(subRest)))
      //         ),
      //         (sub: Sub)   => cSuperMid.inverse(Right(midSuperSub.inverse(Right(sub))))
      //       )
      //   }
      //   // val initMapper: coproduct.Mapper.Aux[executors.type, InitWorkTypeUnion, InitProgram#Result] =
      //   //   chainedMapper[InitWorkTypeUnion]
      //   val initMapper = m
      //   val initPrepend: coproduct.Prepend[embedBasis.Rest, initEmbedBasis.Rest] =
      //     implicitly[coproduct.Prepend[embedBasis.Rest, initEmbedBasis.Rest]]
      //   val initToCEmbedBasis: coproduct.Basis[C, InitWorkTypeUnion] =
      //     combinedEmbedBasis[
      //       WorkTypeUnion,
      //       InitWorkTypeUnion,
      //       embedBasis.Rest,
      //       initEmbedBasis.Rest,
      //       initPrepend.Out
      //     ](initEmbedBasis, embedBasis, implicitly[coproduct.Basis.Aux[initPrepend.Out, initEmbedBasis.Rest, embedBasis.Rest]], initPrepend)
      //   mapFn(init.run[C, L](executors)(initMapper, initToCEmbedBasis)).run[C, L](executors)
      // }
    }
  }

}
