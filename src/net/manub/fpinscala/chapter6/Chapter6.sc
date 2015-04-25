import net.manub.fpinscala.chapter6._

val rng = SimpleRNG(42)

val (n1, rng1) = rng.nextInt
val (n2, rng2) = rng1.nextInt
val (double, rng3) = Chapter6.nextDouble(rng2)
val (double2, rng4) = Chapter6.nextDouble(rng3)


val ((i1, d1), r1) = Chapter6.intDouble(rng)
val ((d2, i2), r2) = Chapter6.doubleInt(r1)
val ((d11, d12, d13), r3) = Chapter6.double3(r2)

val (l, r) = Chapter6.ints(10)(rng)

