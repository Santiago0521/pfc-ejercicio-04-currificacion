package taller

class Ejercicio() {

  // Punto 1
  def opCurrified(n: Int)(p: Int)(f: (Int, Int) => Int)(g: Int => Int): Int = {
    def recorrer(actual: Int, restante: Int): Int = {
      if (restante == 1) {
        potencia(actual, p)
      } else {
        f(potencia(actual, p), recorrer(g(actual), restante - 1))
      }
    }

    def potencia(base: Int, exp: Int): Int = {
      if (exp == 0) {
        1
      } else {
        base * potencia(base, exp - 1)
      }
    }

    recorrer(1, n)
  }

  // Punto 2
  def suma4(f: Int => Int)(prox: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) {
      0
    } else {
      f(a) + suma4(f)(prox)(prox(a), b)
    }
  }

  def sumaCuadradosSuc: (Int, Int) => Int = {
    (a, b) => suma4(x => x * x)(x => x + 1)(a, b)
  }

  // Punto 3
  def reducirC(op: (Int, Int) => Int)(inicio: Int)
              (f: Int => Int, prox: Int => Int)
              (a: Int, b: Int): Int = {
    if (a > b) {
      inicio
    } else {
      op(f(a), reducirC(op)(inicio)(f, prox)(prox(a), b))
    }
  }

  def producto(f: Int => Int, prox: Int => Int, a: Int, b: Int): Int = {
    reducirC((x, y) => x * y)(1)(f, prox)(a, b)
  }

  def factorialHOF(n: Int): Int = {
    producto(x => x, x => x + 1, 1, n)
  }

  // Punto 4
  def componer(f: Int => Int)(g: Int => Int): Int => Int = {
    (x: Int) => f(g(x))
  }

  def aplicarN(f: Int => Int)(n: Int): Int => Int = {
    if (n == 0) {
      (x: Int) => x
    } else {
      (x: Int) => f(aplicarN(f)(n - 1)(x))
    }
  }

  def sumador(n: Int): Int => Int = {
    (x: Int) => x + n
  }

}