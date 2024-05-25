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

  /*
  def itinerarioSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[Itinerario] = {
    (c1: String, c2: String, horaCita: Int, minutoCita: Int) => {
      // Paso 1: Validar entradas
      if (!aeropuertos.exists(_.Cod == c1) || !aeropuertos.exists(_.Cod == c2)) {
        println(s"Códigos de aeropuerto inválidos: $c1, $c2")
        List.empty[Itinerario]
      } else {
        // Paso 2: Filtrar vuelos de c1 a c2 que lleguen antes de la hora de la cita
        val vuelosFiltrados = vuelos.filter(v => v.Org == c1 && v.Dst == c2 &&
          (v.HL < horaCita || (v.HL == horaCita && v.ML <= minutoCita)))

        // Paso 3: Crear itinerarios
        val itinerarios: List[Itinerario] = vuelosFiltrados.map(vuelo => List(vuelo))

        // Paso 4: Devolver los itinerarios
        itinerarios
      }
    }
  }
  */

  /*
  def itinerarioSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[Itinerario] = {
    (c1: String, c2: String, horaCita: Int, minutoCita: Int) => {
      // Paso 1: Validar entradas
      if (!aeropuertos.exists(_.Cod == c1) || !aeropuertos.exists(_.Cod == c2)) {
        println(s"Códigos de aeropuerto inválidos: $c1, $c2")
        List.empty[Itinerario]
      } else {
        // Paso 2: Filtrar vuelos que lleguen antes de la hora de la cita
        val vuelosDirectos = vuelos.filter(v => v.Org == c1 && v.Dst == c2 &&
          (v.HL < horaCita || (v.HL == horaCita && v.ML <= minutoCita)))

        val vuelosConEscala = vuelos.flatMap { v1 =>
          if (v1.Org == c1 && (v1.HL < horaCita || (v1.HL == horaCita && v1.ML <= minutoCita))) {
            vuelos.filter(v2 =>
              v1.Dst == v2.Org && v2.Dst == c2 &&
                ((v2.HS > v1.HL) || (v2.HS == v1.HL && v2.MS > v1.ML)) && // V2 sale después de que V1 llegue
                (v2.HL < horaCita || (v2.HL == horaCita && v2.ML <= minutoCita)) // V2 llega antes de la hora de la cita
            ).map(v2 => List(v1, v2))
          } else {
            List.empty[List[Vuelo]]
          }
        }

        // Paso 3: Combinar itinerarios directos y con escala
        val itinerarios: List[Itinerario] = vuelosDirectos.map(vuelo => List(vuelo)) ++ vuelosConEscala

        // Paso 4: Devolver los itinerarios
        itinerarios
      }
    }
  }
  */

  def itinerarioSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[Itinerario] = {
    (c1: String, c2: String, horaCita: Int, minutoCita: Int) => {
      // Paso 1: Validar entradas
      if (!aeropuertos.exists(_.Cod == c1) || !aeropuertos.exists(_.Cod == c2)) {
        println(s"Códigos de aeropuerto inválidos: $c1, $c2")
        List.empty[Itinerario]
      } else {
        // Paso 2: Filtrar vuelos directos que lleguen antes de la hora de la cita
        val vuelosDirectos = vuelos.filter(v => v.Org == c1 && v.Dst == c2 &&
          (v.HL < horaCita || (v.HL == horaCita && v.ML <= minutoCita)))

        // Paso 3: Filtrar vuelos con una escala que lleguen antes de la hora de la cita
        val vuelosConEscala = vuelos.flatMap { v1 =>
          if (v1.Org == c1 && (v1.HL < horaCita || (v1.HL == horaCita && v1.ML <= minutoCita))) {
            vuelos.filter(v2 =>
              v1.Dst == v2.Org && v2.Dst == c2 &&
                ((v2.HS > v1.HL) || (v2.HS == v1.HL && v2.MS > v1.ML)) && // V2 sale después de que V1 llegue
                (v2.HL < horaCita || (v2.HL == horaCita && v2.ML <= minutoCita)) // V2 llega antes de la hora de la cita
            ).map(v2 => List(v1, v2))
          } else {
            List.empty[List[Vuelo]]
          }
        }

        // Paso 4: Combinar y ordenar itinerarios por la última hora de salida del aeropuerto de origen
        val itinerarios: List[Itinerario] = (vuelosDirectos.map(vuelo => List(vuelo)) ++ vuelosConEscala)
          .sortBy(it => it.head.HS * 60 + it.head.MS)(Ordering[Int].reverse)

        // Paso 5: Devolver el itinerario con la última hora de salida posible que llegue a tiempo
        itinerarios.headOption.toList
      }
    }
  }


}
