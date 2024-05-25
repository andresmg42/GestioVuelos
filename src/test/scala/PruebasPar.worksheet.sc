import common._
import Itinerarios._
import Datos._

import scala.collection.parallel.CollectionConverters._

// Función que genera itinerarios paralelos
def itinerariosPar(
    vuelos: List[Vuelo],
    aeropuertos: List[Aeropuerto]
): (String, String) => List[Itinerario] = {

  // Crear un mapa de aeropuertos para acceso rápido
  val aeropuertosMap = aeropuertos.map(airport => airport.Cod -> airport).toMap

  // Función recursiva para generar itinerarios
  def generarItinerarios(
      cod1: String,
      cod2: String,
      visitados: Set[String]
  ): List[Itinerario] = {
    // Caso base: si el aeropuerto de origen y destino son el mismo
    if (cod1 == cod2) 
      List(Nil)
    else {
      // Filtrar los vuelos que salen del aeropuerto de origen
      val vuelosDesdeCod1 = vuelos.filter(_.Org == cod1) 
      // Mapear y ejecutar en paralelo la generación de itinerarios para cada vuelo
      val result = vuelosDesdeCod1.map { vuelo =>
        task {
          if (!visitados(vuelo.Dst)) {
            // Generar itinerarios desde el destino actual del vuelo
            val itinerariosRestantes = generarItinerarios(vuelo.Dst, cod2, visitados + vuelo.Dst)
            itinerariosRestantes.map(vuelo :: _)
          } else {
            Nil
          }
        }
      }
      // Unir los resultados de las tareas paralelas
      result.flatMap(_.join())
    }
  }

  // Función resultante que valida la existencia de los aeropuertos y genera itinerarios
  (cod1: String, cod2: String) => {
    if (aeropuertosMap.contains(cod1) && aeropuertosMap.contains(cod2))
      generarItinerarios(cod1, cod2, Set(cod1))
    else 
      Nil
  }
}

// Función que genera y ordena itinerarios en función del tiempo usando colección paralela
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


