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

/**
 * Genera los itinerarios entre dos aeropuertos ordenados por número de escalas, utilizando procesamiento en paralelo.
 * 
 * @param vuelos Lista de todos los vuelos disponibles.
 * @param aeropuertos Lista de aeropuertos con su información.
 * @return Función que devuelve una lista de itinerarios ordenados por número de escalas entre dos aeropuertos.
 */
def itinerariosEscalasPar(
    vuelos: List[Vuelo],
    aeropuertos: List[Aeropuerto]
): (String, String) => List[Itinerario] = {

  /**
   * Función auxiliar para calcular la suma de escalas para cada itinerario.
   * 
   * @param it Lista de itinerarios.
   * @return Lista de tuplas con la suma de escalas y los itinerarios.
   */
  def aux(it: List[Itinerario]): List[(Int, Itinerario)] = {
    it.map(i => (i.foldLeft(0)(_ + _.Esc) + (i.length - 1), i))
  }

  // Función principal para generar itinerarios entre dos aeropuertos
  (cod1: String, cod2: String) => {
    // Obtener todos los itinerarios posibles entre los aeropuertos dados
    val It = itinerariosPar(vuelos, aeropuertos)(cod1, cod2)
    // Dividir la lista de itinerarios en cuatro partes iguales
    val ItA = It.slice(0, It.length / 4)
    val ItB = It.slice(It.length / 4, It.length * 2 / 4)
    val ItC = It.slice(It.length * 2 / 4, It.length * 3 / 4)
    val ItD = It.slice(It.length * 3 / 4, It.length)

    if (It.isEmpty) Nil // Si no hay itinerarios, devolver una lista vacía
    else {
      // Procesar cada sublista de itinerarios en paralelo y combinar los resultados
      val (l1, l2, l3, l4) = parallel(aux(ItA), aux(ItB), aux(ItC), aux(ItD))
      // Combinar los resultados y ordenar por la suma de escalas, tomando los primeros 3 itinerarios
      (l1 ++ l2 ++ l3 ++ l4).sortBy(t => t._1).map(t => t._2).take(3)
    }
  }
}

/**
 * Genera los itinerarios entre dos aeropuertos ordenados por tiempo de vuelo, utilizando procesamiento en paralelo.
 * 
 * @param vuelos Lista de todos los vuelos disponibles.
 * @param aeropuertos Lista de aeropuertos con su información.
 * @return Función que devuelve una lista de itinerarios ordenados por tiempo de vuelo entre dos aeropuertos.
 */
def itinerariosAirePar(
    vuelos: List[Vuelo],
    aeropuertos: List[Aeropuerto]
): (String, String) => List[Itinerario] = { (c1: String, c2: String) =>
  {
    // Obtener todos los itinerarios posibles entre los aeropuertos dados y paralelizar el procesamiento
    val itinerarioOpc = itinerariosPar(vuelos, aeropuertos)(c1, c2).par
    // Calcular el tiempo de vuelo para cada itinerario, ordenar por tiempo de vuelo y tomar los primeros 3
    val tiemposTotales = itinerarioOpc
      .map(it => (tiempoVueloIt(it, aeropuertos), it)) // Calcular el tiempo de vuelo para cada itinerario
      .seq // Convertir de vuelta a una secuencia secuencial
      .sortBy(_._1) // Ordenar por tiempo de vuelo
      .map(_._2) // Extraer los itinerarios
      .take(3) // Tomar los primeros 3 itinerarios
    tiemposTotales.toList // Convertir de vuelta a una lista
  }

  
}


// Definición de la función itinerarioSalidaPar que recibe una lista de vuelos y una lista de aeropuertos.
// Retorna una función anónima que toma cuatro parámetros: cod1, cod2, H y M, y devuelve un Itinerario.
def itinerarioSalidaPar(
    vuelos: List[Vuelo],
    aeropuertos: List[Aeropuerto]
): (String, String, Int, Int) => Itinerario = {

  // Crea un mapa donde la clave es el código del aeropuerto y el valor es el objeto Aeropuerto.
  val aeropuertosMap = aeropuertos.map(airport => airport.Cod -> airport).toMap

  // Retorna una función anónima que toma cuatro parámetros.
  (cod1: String, cod2: String, H: Int, M: Int) => {

    // Función auxiliar que procesa una lista de itinerarios.
    def aux(itHL: List[Itinerario]) = {
      itHL.map(it => {
        // Convierte la hora y minuto de salida a GMT.
        val (hs, ms) = convertirHorasGMT(
          it.head.HS,
          it.head.MS,
          aeropuertosMap(cod1).GMT / 100
        )
        // Convierte la hora y minuto de llegada a GMT.
        val (hl, ml) = convertirHorasGMT(
          it.last.HL,
          it.last.ML,
          aeropuertosMap(cod2).GMT / 100
        )
        // Calcula la diferencia entre la hora de llegada y la hora de salida.
        val (h, m) = sumarHoras(hl, ml, hs, ms, '-')
        // Retorna un par que contiene el itinerario y la diferencia en minutos.
        (it, h * 60 + m)
      })
    }

    // Obtiene los itinerarios posibles entre los aeropuertos cod1 y cod2.
    val It = itinerariosPar(vuelos, aeropuertos)(cod1, cod2)
    
    // Filtra los itinerarios para aquellos cuya hora de llegada es menor o igual a H y cuyo minuto de llegada es menor o igual a M.
    val itHL = It.filter(it => it.last.HL <= H && it.last.ML <= M)
    
    // Si no hay itinerarios que cumplan con el filtro, retorna una lista vacía.
    if (itHL.isEmpty) Nil
    else {
      // Divide la lista de itinerarios filtrados en cuatro partes.
      val itA = itHL.slice(0, itHL.length / 4)
      val itB = itHL.slice(itHL.length / 4, itHL.length / 2)
      val itC = itHL.slice(itHL.length / 2, itHL.length * 3 / 4)
      val itD = itHL.slice(itHL.length * 3 / 4, itHL.length)

      // Procesa las cuatro partes en paralelo utilizando la función auxiliar `aux`.
      val (ita, itb, itc, itd) = parallel(aux(itA), aux(itB), aux(itC), aux(itD))
      
      // Combina los resultados de las cuatro partes, ordena por la diferencia en minutos y retorna el primer itinerario (el que tiene la menor diferencia).
      (ita ++ itb ++ itc ++ itd).sortBy(t => t._2).map(t => t._1).head
    }
  }
}

