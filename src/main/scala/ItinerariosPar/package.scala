import common._
import scala.collection.parallel.CollectionConverters._
import Itinerarios._
import Datos._
package object ItinerariosPar {

  def itinerariosPar(
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
        val result = vuelosDesdeCod1.map { vuelo =>
          task {
            if (!visitados(vuelo.Dst)) {
              val itinerariosRestantes =
                generarItinerarios(vuelo.Dst, cod2, visitados + vuelo.Dst)
              itinerariosRestantes.map(vuelo :: _)
            } else {
              Nil
            }
          }
        }

        result.flatMap(_.join())
      }
    }

    (cod1: String, cod2: String) => {

      if (aeropuertosMap.contains(cod1) && aeropuertosMap.contains(cod2))
        generarItinerarios(cod1, cod2, Set(cod1))
      else Nil
    }
  }

  def itinerariosTiempoPar(
      vuelos: List[Vuelo],
      aeropuertos: List[Aeropuerto]
  ): (String, String) => List[Itinerario] = {

    def aux(
        it: List[Itinerario],
        aeropuertos: List[Aeropuerto]
    ): List[(Int, List[Vuelo])] = {
      val tiemposIt = for {
        i <- it

      } yield (tiempoVueloIt(i, aeropuertos) + tiempoEsperaIt(i), i)
      tiemposIt

    }

    (cod1: String, cod2: String) => {
      val it = itinerariosPar(vuelos, aeropuertos)(cod1, cod2)
      val itA = it.slice(0, it.length / 4)
      val itB = it.slice(it.length / 4, it.length / 2)
      val itC = it.slice(it.length / 2, it.length * 3 / 4)
      val itD = it.slice(it.length * 3 / 4, it.length)
      if (it.isEmpty) Nil
      else {
        val (ita, itb, itc, itd) = parallel(
          aux(itA, aeropuertos),
          aux(itB, aeropuertos),
          aux(itC, aeropuertos),
          aux(itD, aeropuertos)
        )
        val Tiempos = ita ++ itb ++ itc ++ itd
        val itsTiempo = Tiempos.sortBy(t => t._1).map(t => t._2)
        (for {
          i <- 0 until itsTiempo.length
          if i <= 2
        } yield itsTiempo(i)).toList
      }

    }

  }

  def itinerariosTiempoParCol(
      vuelos: List[Vuelo],
      aeropuertos: List[Aeropuerto]
  ): (String, String) => List[Itinerario] = { 
    
    (cod1: String, cod2: String) =>
    {
      val it = itinerariosPar(vuelos, aeropuertos)(cod1, cod2).par
      if (it.isEmpty) Nil
      else {
        val tiemposIt = for {
          i <- it

        } yield (tiempoVueloIt(i, aeropuertos) + tiempoEsperaIt(i), i)

        val itsTiempo = tiemposIt.seq.sortBy(t => t._1).map(t => t._2)
        (for {
          i <- 0 until itsTiempo.length
          if i <= 2
        } yield itsTiempo(i)).toList
      }

    }

  }

}
