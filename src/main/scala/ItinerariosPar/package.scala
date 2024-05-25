import common._
import scala.util.Random
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.CollectionConverters._
import Benchmark._
import Itinerarios._
import Datos._
package object ItinerariosPar {
  
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

  /* NOTA: EN ItineriosAirePar1 se paralelizan las dos funciones. Sin embargo, al realizar 
  las pruebas itinerariosAirePar2 (se emplea '.par' en la funcion anonima que retorna), es mas rapida
  por dos segundo ¿deberai de dejarse la segunda version o es necesario emplear task?*/ 

    
  def itinerariosAirePar1( vuelos : List[Vuelo] , aeropuertos:List[Aeropuerto] ) : ( String , String ) =>List [ Itinerario ]= {
    // Recibe vuelos , una lista de todos los vuelos disponibles y
    // aeropuertos una lista de todos los aeropuertos
    // y devuelve una funcion que recibe c1 y c2 , codigos de aeropuertos
    // y devuelve los tres ( si los hay ) itinerarios que minimizan el tiempo en el aire entre esos dos aeropuertos
    
    
    def tiempoItinerarioPar1( itinerario: List[Vuelo] ): (Int) =
    {

      val aeropuertosMap =aeropuertos.map(airport => airport.Cod -> airport).toMap
      val tiempoVuelos = itinerario.map( vuelo => task{
        val GMTOrg = aeropuertosMap(vuelo.Org).GMT
        val GMTDst = aeropuertosMap(vuelo.Dst).GMT
        val horaSalida = convertirHorasGMT(vuelo.HS, vuelo.MS , GMTOrg)
        val horaLLegada = convertirHorasGMT(vuelo.HL, vuelo.ML , GMTDst)
        sumarHoras(horaSalida._1 , horaSalida._2 , horaLLegada._1 , horaLLegada._2 , '-')
      }.join())
      tiempoVuelos.map(t => (t._1 * 60) + t._2).sum
    }


    (c1: String , c2: String)=>{
      val itinerarioOpc = itinerariosPar( vuelos, aeropuertos)(c1,c2).par
      val tiemposTotales = itinerarioOpc.map( it =>
        (tiempoItinerarioPar1(it), it)
      ).seq.sortBy(_._1).map(_._2).take(3)
      tiemposTotales.toList
    }
  }
  def itinerariosAirePar2( vuelos : List[Vuelo] , aeropuertos:List[Aeropuerto] ) : ( String , String ) =>List [ Itinerario ]= {
    // Recibe vuelos , una lista de todos los vuelos disponibles y
    // aeropuertos una lista de todos los aeropuertos
    // y devuelve una funcion que recibe c1 y c2 , codigos de aeropuertos
    // y devuelve los tres ( si los hay ) itinerarios que minimizan el tiempo en el aire entre esos dos aeropuertos
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
      val itinerarioOpc = itinerariosPar( vuelos, aeropuertos)(c1,c2).par
      val tiemposTotales = itinerarioOpc.map( it =>
        (tiempoItinerario(it), it)
      ).seq.sortBy(_._1).map(_._2).take(3)
      tiemposTotales.toList
    }
  }

}






