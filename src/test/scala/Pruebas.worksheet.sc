import Datos._

//Itinerarios normal--------------------------------------------------------------------------------------------------------------------
    """
    funcion itinerarios:
    Esta función retorna todos los itinerarios que salen de cod1 hasta cod2

    Args:
        vuelos(List[Vuelo]: lista de objetos de tipo Vuelo
        aeropuertos(List[Aeropuerto]): lista de objetos de tipo Aeropuerto

    Returns:
        List[Itinerario]=List(List[Vuelo]): una lista  de Itinerarios de los vuelos que parten de cod1 hasta cod2   """


def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
  // Crear un mapa de aeropuertos para un acceso más rápido
  val aeropuertosMap = aeropuertos.map(airport => airport.Cod -> airport).toMap

  def generarItinerarios(cod1: String, cod2: String, visitados: Set[String]): List[Itinerario] = {
    if (cod1 == cod2) // Si se llega al destino, devolver una lista vacía
      List(Nil)
    else {
      val vuelosDesdeCod1 = vuelos.filter(_.Org == cod1) // Filtrar vuelos que salen desde cod1
      vuelosDesdeCod1.flatMap { vuelo =>
        if (!visitados(vuelo.Dst)) {
          val itinerariosRestantes = generarItinerarios(vuelo.Dst, cod2, visitados + vuelo.Dst)
          itinerariosRestantes.map(vuelo :: _)
        } else {
          Nil // Si ya se visitó el aeropuerto de destino, no se incluye este vuelo en el itinerario
        }
      }
    }
  }

  // Función final que devuelve los itinerarios dados los códigos de los aeropuertos
  (cod1: String, cod2: String) => {
        //verificar si los aeropuertos existen
        if (aeropuertosMap.contains(cod1) && aeropuertosMap.contains(cod2))
        generarItinerarios(cod1, cod2, Set(cod1))
        else Nil // Si alguno de los aeropuertos no existe, devolver una lista vacía
    }
  }


    """
    funcion sumarHoras:
    Esta función suma y resta horas en formato 24H.

    Args:
        h1:Int->Hora antes de el simbolo operador
        m1:Int->minutos antes del simbolo operador
        h2:Int->hora despues del operador
        m2:Int->minutos despues del operador
        op:Int->operador('+','-')

    Returns:
        (Int,Int): tupla que contienen en la primera pocicion la hora y en la segunda los minutos de haber sumado o restado dos horas (H:M) determinadas  """

def sumarHoras(h1: Int, m1: Int, h2: Int, m2: Int, op: Char): (Int, Int) = {
  // Convertir la primera hora y minutos en total de minutos
  val H1 = h1 * 60 + m1 
  // Convertir la segunda hora y minutos en total de minutos
  val H2 = h2 * 60 + m2 
  // Aplicar el operador para sumar o restar los minutos totales
  val suma = op match {
    case '+' => H1 + H2  // Sumar los minutos totales
    case '-' => H1 - H2  // Restar los minutos totales
  }
  // Si el resultado de la operación es negativo, ajustarlo para que sea positivo dentro del rango de un día (1440 minutos)
  if (suma < 0) {
    val result = suma + 1440  // Ajustar el tiempo negativo sumando un día completo en minutos

    // Devolver las horas y minutos correspondientes del resultado ajustado
    (result / 60, result % 60)  
  } else {
    // Si el resultado no es negativo, simplemente devolver las horas y minutos del resultado
    (suma / 60, suma % 60)
  }
}

   """
    funcion convertirHorasGMT:
    Esta función suma y resta horas en formato 24H.

    Args:
        h:Int->hora local
        m:Int->minutos locales
        gmt:Int->uso Horario GMT 

    Returns:
        (Int,Int): tupla que contienen en la primera pocicion la hora y en la segunda los minutos convertidos a formato GMT con uso horario (0) """

//si gmt es menor que 0, sumamos el valor absoluto del uso horario sino restamos el uso horario normal a la hora local, cave resaltar que gmt es de un solo digito.

def convertirHorasGMT(h: Int, m: Int, gmt: Int): (Int, Int) =if (gmt < 0) sumarHoras(h, m, -gmt, 0, '+') else sumarHoras(h, m, gmt, 0, '-')


   """
    funcion tiempoVueloIt:
    Esta funcion calcula el tiempo de vuelo de un determinado Itinerario.

    Args:
        itinerario:Itinerario-> lista de vuelos 
        aeropuertos:List[Aeropuerto]-> lista de aeropuertos

    Returns:
        Int: entero que representa el tiempo total de vuelo de un determinado itienerario """

def tiempoVueloIt(
    itinerario: Itinerario,
    aeropuertos: List[Aeropuerto]
): Int = {
  // Crear un mapa de códigos de aeropuerto a objetos de aeropuerto para acceso rápido
  val aeropuertosMap = aeropuertos.map(airport => airport.Cod -> airport).toMap

  // Calcular el tiempo de vuelo para cada vuelo en el itinerario
  val horasViajeItinerario = itinerario.map(vuelo => {
    // Obtener el uso horario GMT de el origen del vuelo
    val gmtO = aeropuertosMap(vuelo.Org).GMT / 100
    // Obtener el suo horario GMT de el destino del vuelo
    val gmtD = aeropuertosMap(vuelo.Dst).GMT / 100
    // Convertir la hora de salida a GMT
    val HGMTO = convertirHorasGMT(vuelo.HS, vuelo.MS, gmtO)
    // Convertir la hora de llegada a GMT
    val HGMTD = convertirHorasGMT(vuelo.HL, vuelo.ML, gmtD)
    // Calcular la diferencia entre la hora de llegada y la hora de salida en GMT
    sumarHoras(HGMTD._1, HGMTD._2, HGMTO._1, HGMTO._2, '-')
  })

  // Convertir cada tiempo de vuelo (horas, minutos) a minutos y sumarlos todos para obtener el tiempo total de vuelo
  horasViajeItinerario.map(hora => hora._1 * 60 + hora._2).sum
}


"""
    funcion tiempoEsperaIt:
    Esta funcion calcula el tiempo de Espera de un determinado Itinerario.

    Args:
        itinerario:Itinerario-> lista de vuelos 

    Returns:
        Int: entero que representa el tiempo total de Espera de un determinado itienerario """


def tiempoEsperaIt(itinerario: Itinerario): Int = {
  // Si el itinerario está vacío, el tiempo de espera es 0
  if (itinerario.isEmpty) 0
  else {
    // Calcular el tiempo de espera entre cada par de vuelos consecutivos
    val result = (0 until itinerario.length - 1).map(i => {
      val v = itinerario(i)
      val vNext = itinerario(i + 1)
      // Calcular la diferencia entre la hora de salida del siguiente vuelo y la hora de llegada del vuelo actual
      sumarHoras(vNext.HS, vNext.MS, v.HL, v.ML, '-')
    }).toList

    // Convertir cada tiempo de espera (horas, minutos) a minutos y sumarlos todos para obtener el tiempo total de espera
    result.map(hora => hora._1 * 60 + hora._2).sum
  }
}


"""
    funcion itinerariosTiempo:
    Esta funcion calcula los tres itinerarios(si los hay) que tienen menos tiempo total de viaje entre el destino y origen.

    Args:
        itinerario:Itinerario-> lista de vuelos
        aeropuertos:List['Aeropuerto']->lista de aeropuertos 

    Returns:
        Int: entero que representa el tiempo total de Espera de un determinado itienerario """

def itinerariosTiempo(
    vuelos: List[Vuelo],        // Parámetro: lista de vuelos
    aeropuertos: List[Aeropuerto]   // Parámetro: lista de aeropuertos
): (String, String) => List[Itinerario] = {  // La función devuelve una función que toma dos Strings y devuelve una lista de Itinerarios
  
  (cod1: String, cod2: String) =>    // Definición de la función anónima que toma dos códigos de aeropuerto
  {
    val it = itinerarios(vuelos, aeropuertos)(cod1, cod2)   // Se calculan los itinerarios disponibles entre los dos aeropuertos usando la función "itinerarios"
    if (it.isEmpty) Nil   // Si no hay itinerarios disponibles, se devuelve una lista vacía

    else{
      val tiemposIt = for {
        i <- it   // Se itera sobre cada itinerario en la lista "it"
      } yield (tiempoVueloIt(i,aeropuertos)+tiempoEsperaIt(i), i)   // Se calcula el tiempo total de vuelo y espera para cada itinerario, y se almacena junto con el itinerario

      tiemposIt.sortBy(t=>t._1).map(t=>t._2).take(3)   // Se ordenan los itinerarios según el tiempo total de vuelo y espera, se eliminan los tiempos y se toman los primeros tres itinerarios
    }
  }
}

/**
 * Genera los itinerarios entre dos aeropuertos ordenados por número de escalas.
 * 
 * @param vuelos Lista de todos los vuelos disponibles.
 * @param aeropuertos Lista de aeropuertos con su información.
 * @return Función que devuelve una lista de itinerarios ordenados por número de escalas entre dos aeropuertos.
 */
def itinerariosEscalas(
    vuelos: List[Vuelo],
    aeropuertos: List[Aeropuerto]
): (String, String) => List[Itinerario] = { (cod1: String, cod2: String) =>
  {
    // Obtener todos los itinerarios posibles entre los aeropuertos dados
    val It = itinerarios(vuelos, aeropuertos)(cod1, cod2)
    if (It.isEmpty) Nil // Si no hay itinerarios, devolver una lista vacía
    else {
      // Calcular la suma de escalas para cada itinerario y ordenar por esta suma
      val sumaEsc =
        It.map(i => (i.foldLeft(0)(_ + _.Esc) + (i.length - 1), i)) // Sumar las escalas de cada vuelo en el itinerario
      sumaEsc.sortBy(t => t._1).map(t => t._2).take(3) // Ordenar por la suma de escalas y tomar los primeros 3 itinerarios
    }
  }
}

/**
 * Genera los itinerarios entre dos aeropuertos ordenados por tiempo de vuelo.
 * 
 * @param vuelos Lista de todos los vuelos disponibles.
 * @param aeropuertos Lista de aeropuertos con su información.
 * @return Función que devuelve una lista de itinerarios ordenados por tiempo de vuelo entre dos aeropuertos.
 */
def itinerariosAire(
    vuelos: List[Vuelo],
    aeropuertos: List[Aeropuerto]
): (String, String) => List[Itinerario] = { (c1: String, c2: String) =>
  {
    // Obtener todos los itinerarios posibles entre los aeropuertos dados
    val itinerarioOpc = itinerarios(vuelos, aeropuertos)(c1, c2)
    if (itinerarioOpc.isEmpty) Nil // Si no hay itinerarios, devolver una lista vacía
    else {
      // Ordenar los itinerarios por tiempo de vuelo y tomar los primeros 3
      itinerarioOpc
        .map(it => (tiempoVueloIt(it, aeropuertos), it)) // Calcular el tiempo de vuelo para cada itinerario
        .sortBy(_._1) // Ordenar por tiempo de vuelo
        .map(_._2) // Extraer los itinerarios
        .take(3) // Tomar los primeros 3 itinerarios
    }
  }

  
}

// Definición de la función itinerarioSalida que recibe una lista de vuelos y una lista de aeropuertos.
// Retorna una función anónima que toma cuatro parámetros: cod1, cod2, H y M, y devuelve un Itinerario.
def itinerarioSalida(
      vuelos: List[Vuelo],
      aeropuertos: List[Aeropuerto]
  ): (String, String, Int, Int) => Itinerario = {

    // Crea un mapa donde la clave es el código del aeropuerto y el valor es el objeto Aeropuerto.
    val aeropuertosMap =
      aeropuertos.map(airport => airport.Cod -> airport).toMap

    // Retorna una función anónima que toma cuatro parámetros.
    (cod1: String, cod2: String, H: Int, M: Int) => {
      
      // Obtiene los itinerarios posibles entre los aeropuertos cod1 y cod2.
      val It = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      
      // Filtra los itinerarios para aquellos cuya hora de llegada es menor o igual a H y cuyo minuto de llegada es menor o igual a M.
      val itHL = It.filter(it => it.last.HL <= H && it.last.ML <= M)
      
      // Si no hay itinerarios que cumplan con el filtro, retorna una lista vacía.
      if (itHL.isEmpty) Nil
      else {
        // Calcula la diferencia en minutos entre la hora de salida y la hora de llegada para cada itinerario filtrado.
        val difHorasit = itHL.map(it => {
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
          // Retorna un par que contiene la diferencia en minutos y el itinerario.
          ( h * 60 + m, it)
        })
        
        // Ordena los itinerarios por la diferencia en minutos y retorna el primero (el que tiene la menor diferencia).
        difHorasit.sortBy(t => t._1).map(t => t._2).head
      }
    }
  }












