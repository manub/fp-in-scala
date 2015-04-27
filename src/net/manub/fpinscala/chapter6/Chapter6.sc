import net.manub.fpinscala.chapter6._
import net.manub.fpinscala.chapter6.RNG._

val rng = RNG.SimpleRNG(42)

val (n1, rng1) = rng.nextInt
val (n2, rng2) = rng1.nextInt
val (double, rng3) = RNG.nextDouble(rng2)
val (double2, rng4) = RNG.nextDouble(rng3)
val ((i1, d1), r1) = RNG.intDouble(rng)
val ((d2, i2), r2) = RNG.doubleInt(r1)
val ((d11, d12, d13), r3) = RNG.double3(r2)
val (l, r) = RNG.ints(10)(rng)

val zero = rollDie(SimpleRNG(5))._1
