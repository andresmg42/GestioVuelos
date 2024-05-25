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

// Función que genera y ordena itinerarios en función del tiempo
def itinerariosTiempoPar(
    vuelos: List[Vuelo],
    aeropuertos: List[Aeropuerto]
): (String, String) => List[Itinerario] = { 

  // Función auxiliar que calcula el tiempo total de cada itinerario
  def aux(it: List[Itinerario], aeropuertos: List[Aeropuerto]): List[(Int, List[Vuelo])] = {
    val tiemposIt = for {
      i <- it
    } yield (tiempoVueloIt(i, aeropuertos) + tiempoEsperaIt(i), i)
    tiemposIt
  }
  
  // Función resultante que genera y ordena itinerarios
  (cod1: String, cod2: String) => {
    val it = itinerariosPar(vuelos, aeropuertos)(cod1, cod2)
    
    // Dividir los itinerarios en cuatro partes para procesarlos en paralelo
    val itA = it.slice(0, it.length / 4)
    val itB = it.slice(it.length / 4, it.length / 2)
    val itC = it.slice(it.length / 2, it.length * 3 / 4)
    val itD = it.slice(it.length * 3 / 4, it.length)
    
    if (it.isEmpty) Nil
    else {
      // Procesar las partes en paralelo y unir los resultados
      val (ita, itb, itc, itd) = parallel(aux(itA, aeropuertos), aux(itB, aeropuertos), aux(itC, aeropuertos), aux(itD, aeropuertos))
      val tiempos = ita ++ itb ++ itc ++ itd
      // Ordenar los itinerarios por tiempo y tomar los tres primeros
      val itsTiempo = tiempos.sortBy(t => t._1).map(t => t._2)
      (for {
        i <- 0 until itsTiempo.length
        if i <= 2
      } yield itsTiempo(i)).toList
    }
  }
}

// Función que genera y ordena itinerarios en función del tiempo usando colección paralela
def itinerariosTiempoParCol(
    vuelos: List[Vuelo],
    aeropuertos: List[Aeropuerto]
): (String, String) => List[Itinerario] = { 
  
  (cod1: String, cod2: String) => {
    val it = itinerariosPar(vuelos, aeropuertos)(cod1, cod2).par
    
    if (it.isEmpty) Nil
    else {
      // Calcular el tiempo total para cada itinerario en paralelo
      val tiemposIt = for {
        i <- it
      } yield (tiempoVueloIt(i, aeropuertos) + tiempoEsperaIt(i), i)
      
      // Ordenar los itinerarios por tiempo y tomar los tres primeros
      val itsTiempo = tiemposIt.seq.sortBy(t => t._1).map(t => t._2)
      (for {
        i <- 0 until itsTiempo.length
        if i <= 2
      } yield itsTiempo(i)).toList
    }
  }
}


