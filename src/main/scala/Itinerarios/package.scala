import Datos._

package object Itinerarios {

  //-------------funciones auxiliares-------------------------------------------

  def sumarHoras(h1: Int, m1: Int, h2: Int, m2: Int, op: Char): (Int, Int) = {
    val H1 = h1 * 60 + m1
    val H2 = h2 * 60 + m2
    val suma = op match {
      case '+' => H1 + H2
      case '-' => H1 - H2
    }
    if (suma < 0) {
      val result = suma + 1440

      (result / 60, result % 60)
    } else (suma / 60, suma % 60)
  }

  def convertirHorasGMT(h: Int, m: Int, gmt: Int): (Int, Int) = if (gmt < 0)
    sumarHoras(h, m, -gmt, 0, '+')
  else sumarHoras(h, m, gmt, 0, '-')

  def tiempoVueloIt(
      itinerario: Itinerario,
      aeropuertos: List[Aeropuerto]
  ): Int = {

    val aeropuertosMap =
      aeropuertos.map(airport => airport.Cod -> airport).toMap
    itinerario
      .map(vuelo => {
        val gmtO = aeropuertosMap(vuelo.Org).GMT / 100
        val gmtD = aeropuertosMap(vuelo.Dst).GMT / 100
        val HGMTO = convertirHorasGMT(vuelo.HS, vuelo.MS, gmtO)
        val HGMTD = convertirHorasGMT(vuelo.HL, vuelo.ML, gmtD)
        val (h, m) = sumarHoras(HGMTD._1, HGMTD._2, HGMTO._1, HGMTO._2, '-')
        h * 60 + m

      })
      .sum

  }

  def tiempoEsperaIt(itinerario: Itinerario): Int = {
    if (itinerario.isEmpty) 0
    else {
      (0 until itinerario.length - 1)
        .map(i => {
          val v = itinerario(i)
          val vNext = itinerario(i + 1)
          val (h, m) = sumarHoras(vNext.HS, vNext.MS, v.HL, v.ML, '-')
          h * 60 + m

        })
        .sum

    }
  }

  //------------------- funciones solicitadas----------------------------------------------------------------------------

  def itinerarios(
      vuelos: List[Vuelo],
      aeropuertos: List[Aeropuerto]
  ): (String, String) => List[Itinerario] = {

    val aeropuertosMap =
      aeropuertos.map(airport => airport.Cod -> airport).toMap

    def generarItinerarios(
        cod1: String,
        cod2: String,
        visitados: Set[String]
    ): List[Itinerario] = {
      if (cod1 == cod2)
        List(Nil)
      else {
        val vuelosDesdeCod1 = vuelos.filter(_.Org == cod1)
        vuelosDesdeCod1.flatMap { vuelo =>
          if (!visitados(vuelo.Dst)) {
            val itinerariosRestantes =
              generarItinerarios(vuelo.Dst, cod2, visitados + vuelo.Dst)
            itinerariosRestantes.map(vuelo :: _)
          } else {
            Nil
          }
        }
      }
    }

    (cod1: String, cod2: String) => {

      if (aeropuertosMap.contains(cod1) && aeropuertosMap.contains(cod2))
        generarItinerarios(cod1, cod2, Set(cod1))
      else Nil
    }
  }


  def itinerariosTiempo(
      vuelos: List[Vuelo],
      aeropuertos: List[Aeropuerto]
  ): (String, String) => List[Itinerario] = { (cod1: String, cod2: String) =>
    {
      val it = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      if (it.isEmpty) Nil
      else {
        val tiemposIt = for {
          i <- it

        } yield (tiempoVueloIt(i, aeropuertos) + tiempoEsperaIt(i), i)

        tiemposIt.sortBy(t => t._1).map(t => t._2).take(3)

      }

    }

  }

  def itinerariosEscalas(
      vuelos: List[Vuelo],
      aeropuertos: List[Aeropuerto]
  ): (String, String) => List[Itinerario] = { (cod1: String, cod2: String) =>
    {
      val It = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      if (It.isEmpty) Nil
      else {
        val sumaEsc =
          It.map(i => (i.foldLeft(0)(_ + _.Esc) + (i.length - 1), i))
        sumaEsc.sortBy(t => t._1).map(t => t._2).take(3)
      }
    }

  }

  def itinerariosAire(
      vuelos: List[Vuelo],
      aeropuertos: List[Aeropuerto]
  ): (String, String) => List[Itinerario] = { (c1: String, c2: String) =>
    {
      val itinerarioOpc = itinerarios(vuelos, aeropuertos)(c1, c2)
      if (itinerarioOpc.isEmpty) Nil
      else {
        itinerarioOpc
          .map(it => (tiempoVueloIt(it, aeropuertos), it))
          .sortBy(_._1)
          .map(_._2)
          .take(3)

      }

    }
  }

  def itinerarioSalida(
      vuelos: List[Vuelo],
      aeropuertos: List[Aeropuerto]
  ): (String, String, Int, Int) => Itinerario = {
    val aeropuertosMap =
      aeropuertos.map(airport => airport.Cod -> airport).toMap

    (cod1: String, cod2: String, H: Int, M: Int) => {
      val It = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      val itHL = It.filter(it => it.last.HL <= H && it.last.ML <= M)
      if (itHL.isEmpty) Nil
      else {

        val difHorasit = itHL.map(it => {
          val (hs, ms) = convertirHorasGMT(
            it.head.HS,
            it.head.MS,
            aeropuertosMap(cod1).GMT / 100
          )
          val (hl, ml) = convertirHorasGMT(
            it.last.HL,
            it.last.ML,
            aeropuertosMap(cod2).GMT / 100
          )
          val (h, m) = sumarHoras(hl, ml, hs, ms, '-')
          ( h * 60 + m, it)

        })
        difHorasit.sortBy(t => t._1).map(t => t._2).head
      }

    }
  }


}
