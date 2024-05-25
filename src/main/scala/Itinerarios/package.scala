import common._
import scala.util.Random
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.CollectionConverters._
import Benchmark._
import Datos._
package object Itinerarios {

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
  def convertirHorasGMT(h: Int, m: Int, gmt: Int): (Int, Int) =
    if (gmt < 0) sumarHoras(h, m, -gmt, 0, '+') else sumarHoras(h, m, gmt, 0, '-')
      

//Itinerarios normal--------------------------------------------------------------------------------------------------------------------
  def itinerarios(
      vuelos: List[Vuelo],
      aeropuertos: List[Aeropuerto]
  ): (String, String) => List[Itinerario] = {
    val aeropuertosMap =
      aeropuertos.map(airport => airport.Cod -> airport).toMap
    def formarItinerarios(
        cod1: String,
        cod2: String,
        visitados: Set[String]
    ): List[Itinerario] = {
      if (cod1 == cod2) List(Nil)
      else {
        val vuelosDesdeCod1 = vuelos.filter(_.Org == cod1)
        for {
          v <- vuelosDesdeCod1
          if !visitados(v.Dst)
          itRestante <- formarItinerarios(v.Dst, cod2, visitados + v.Dst)

        } yield v :: itRestante

      }
    }

    (cod1: String, cod2: String) => {
      val aeropuerto1 = aeropuertosMap.get(cod1)
      val aeropuerto2 = aeropuertosMap.get(cod2)
      (aeropuerto1, aeropuerto2) match {
        case (Some(airport1), Some(airport2)) =>
          formarItinerarios(cod1, cod2, Set(cod1))
        case _ =>
          Nil 
      }
    }
  }

  def itinerariosAire(vuelos : List [Vuelo] , aeropuertos: List [Aeropuerto]) : (String , String)=>List[Itinerario]={
    // Recibe vuelos , una lista de todos los vuelos disponibles y
    // aeropuertos una lista de todos los aeropuertos
    // y devuelve una funcion que recibe c1 y c2 , codigos de aeropuertos
    // y devuelve los tres (si los hay) itinerarios que minimizan el tiempo en el aire entre esos dos aeropuertos
    
    def tiempoItinerario( itinerario: List[Vuelo] ): (Int) = 
    {

      val aeropuertosMap =aeropuertos.map(airport => airport.Cod -> airport).toMap
      val tiempoVuelos = itinerario.map( vuelo => {
           val GMTOrg = aeropuertosMap(vuelo.Org).GMT
           val GMTDst = aeropuertosMap(vuelo.Dst).GMT
           val horaSalida = convertirHorasGMT(vuelo.HS, vuelo.MS , GMTOrg)
           val horaLLegada = convertirHorasGMT(vuelo.HL, vuelo.ML , GMTDst)
           sumarHoras(horaSalida._1 , horaSalida._2 , horaLLegada._1 , horaLLegada._2 , '-')
      })
      tiempoVuelos.map( t => ( (t._1)*60) + t._2 ).sum
    }
    (c1: String , c2: String)=>{
      val itinerarioOpc = itinerarios( vuelos, aeropuertos)(c1,c2)
      val tiemposTotales = itinerarioOpc.map( it => 
        (tiempoItinerario(it), it)
      ).sortBy(_._1)
      if(tiemposTotales.length>3)
        List(tiemposTotales(0)._2 , tiemposTotales(1)._2, tiemposTotales(2)._2)
      else
        tiemposTotales.map(it => it._2)
    }
  }

}






