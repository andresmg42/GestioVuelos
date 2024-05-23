import Datos._

package object Itinerarios {

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
    val horasViajeItinerario = itinerario.map(vuelo => {
      val gmtO = aeropuertosMap(vuelo.Org).GMT / 100
      val gmtD = aeropuertosMap(vuelo.Dst).GMT / 100
      val HGMTO = convertirHorasGMT(vuelo.HS, vuelo.MS, gmtO)
      val HGMTD = convertirHorasGMT(vuelo.HL, vuelo.ML, gmtD)
      sumarHoras(HGMTD._1, HGMTD._2, HGMTO._1, HGMTO._2, '-')

    })

    horasViajeItinerario.map(hora => hora._1 * 60 + hora._2).sum

  }

  def tiempoEsperaIt(itinerario: Itinerario): Int = {
    if (itinerario.isEmpty) 0
    else {
      val result = (0 until itinerario.length - 1)
        .map(i => {
          val v = itinerario(i)
          val vNext = itinerario(i + 1)
          sumarHoras(vNext.HS, vNext.MS, v.HL, v.ML, '-')

        })
        .toList

      result.map(hora => hora._1 * 60 + hora._2).sum
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

}
