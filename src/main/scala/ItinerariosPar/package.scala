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
  ): (String, String) => List[Itinerario] = { (cod1: String, cod2: String) =>
    {
      val it = itinerariosPar(vuelos, aeropuertos)(cod1, cod2).par
      if (it.isEmpty) Nil
      else {
        val tiemposIt = for {
          i <- it

        } yield (tiempoVueloIt(i, aeropuertos) + tiempoEsperaIt(i), i)

        tiemposIt.seq.sortBy(t => t._1).map(t => t._2).take(3).toList

      }

    }

  }

  def itinerariosEscalasPar(
      vuelos: List[Vuelo],
      aeropuertos: List[Aeropuerto]
  ): (String, String) => List[Itinerario] = {

    def aux(it: List[Itinerario]): List[(Int, Itinerario)] = {
      it.map(i => (i.foldLeft(0)(_ + _.Esc) + (i.length - 1), i))
    }

    (cod1: String, cod2: String) => {
      val It = itinerariosPar(vuelos, aeropuertos)(cod1, cod2)
      val ItA = It.slice(0, It.length / 4)
      val ItB = It.slice(It.length / 4, It.length * 2 / 4)
      val ItC = It.slice(It.length * 2 / 4, It.length * 3 / 4)
      val ItD = It.slice(It.length * 3 / 4, It.length)

      if (It.isEmpty) Nil
      else {

        val (l1, l2, l3, l4) = parallel(aux(ItA), aux(ItB), aux(ItC), aux(ItD))
        (l1 ++ l2 ++ l3 ++ l4).sortBy(t => t._1).map(t => t._2).take(3)
      }
    }

  }

  def itinerariosAirePar(
      vuelos: List[Vuelo],
      aeropuertos: List[Aeropuerto]
  ): (String, String) => List[Itinerario] = { (c1: String, c2: String) =>
    {
      val itinerarioOpc = itinerariosPar(vuelos, aeropuertos)(c1, c2).par
      val tiemposTotales = itinerarioOpc
        .map(it => (tiempoVueloIt(it, aeropuertos), it))
        .seq
        .sortBy(_._1)
        .map(_._2)
        .take(3)
      tiemposTotales.toList
    }
  }

  def itinerarioSalidaPar(
    vuelos: List[Vuelo],
    aeropuertos: List[Aeropuerto]
): (String, String, Int, Int) => Itinerario = {
  val aeropuertosMap = aeropuertos.map(airport => airport.Cod -> airport).toMap

 
 
  (cod1: String, cod2: String, H: Int, M: Int) => {
     def aux(itHL:List[Itinerario])={
    itHL.map(it => {
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
      (it, h * 60 + m)

    })
  }
  
    val It = itinerariosPar(vuelos, aeropuertos)(cod1, cod2)
    val itHL = It.filter(it => it.last.HL <= H && it.last.ML <= M)
    if (itHL.isEmpty) Nil
    else{
    val itA = itHL.slice(0, itHL.length / 4)
    val itB = itHL.slice(itHL.length / 4, itHL.length / 2)
    val itC = itHL.slice(itHL.length / 2, itHL.length * 3 / 4)
    val itD = itHL.slice(itHL.length * 3 / 4, itHL.length)
    
    val (ita,itb,itc,itd)=parallel(aux(itA),aux(itB),aux(itC),aux(itD)) 
    (ita++itb++itc++itd).sortBy(t => t._2).map(t => t._1).head
    }


  }
}

}
