type Set = Int => Boolean
def singletonSet(elem: Int): Set = x => x == elem

def contains(s: Set, elem: Int): Boolean = s(elem)

def union(s: Set, t: Set): Set =
  (x: Int) => s(x) || t(x)
def intersect(s: Set, t: Set): Set = (x: Int) => s(x) && t(x)
def diff(s: Set, t: Set): Set = (x: Int) => s(x) && !t(x)
def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x)

val bound = 10
def forall(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a > bound) true
    else if (contains(s, a) && !p(a)) false
    else iter(a+1)
  }
  iter(-bound)
}
def exists(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int):Boolean = {
    if (a > bound) false
    else if (contains(s, a) && p(a)) true
    else iter(a+1)
  }
  iter(-bound)
}

val s1: Set = singletonSet(1)
val s2: Set = singletonSet(2)
val s3: Set = singletonSet(3)

def toStr(s: Set): String = {
  val xs = for (i <- -bound to bound if contains(s, i)) yield i
  xs.mkString("{", ",", "}")
}
def printSet(s: Set) {
  println(toStr(s))
}
val s5 = union(s1,s3)
val s4 = intersect(s3,s5)
val s6 = diff(s5,s3)
val s8 = union(s5,s2)
val s7 = filter(s8,x => x%2==1)
val s9 = forall(s7, x => x%2==1)
// printSet(s7)
val s10 = exists(s8, x=>x%2==0)











