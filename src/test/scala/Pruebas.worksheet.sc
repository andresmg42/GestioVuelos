case class Aeropuerto(cod: String, x: Int, y: Int, GMT: Int)

case class Vuelo(
    Aln: String,
    Num: Int,
    Org: String,
    HS: Int,
    MS: Int,
    Dst: String,
    HL: Int,
    ML: Int,
    Esc: Int
)

type Itinerario = List[Vuelo]

val aeropuertosCurso = List(
  Aeropuerto("CLO", 100, 200, -500), // Cali
  Aeropuerto("BOG", 300, 500, -500), // Bogotá
  Aeropuerto("MDE", 200, 600, -500), // Medellin
  Aeropuerto("BAQ", 350, 850, -500), // Barranquilla
  Aeropuerto("SMR", 400, 950, -500), // Santa Marta
  Aeropuerto("CTG", 300, 800, -500), // Cartagena
  Aeropuerto("PTY", 400, 1000, -500), // Ciudad de Panamá
  Aeropuerto("JFK", 2000, 2000, -400), // Nueva York
  Aeropuerto("MIA", 1000, 2000, -500), // Miami
  Aeropuerto("MEX", 1000, 1000, -600), // Ciudad de México
  Aeropuerto("MAD", 5000, 5000, 100), // Madrid
  Aeropuerto("SVCS", 400, 1000, -600), // Caracas
  Aeropuerto("MID", 500, 100, -600), // Merida
  Aeropuerto("AUA", 500, 2000, -400), // Aruba
  Aeropuerto("IST", 9000, 9000, 300), // Estambul
  Aeropuerto("HND", 10000, 12000, 900), // Tokio
  Aeropuerto("DXB", 9500, 11500, 400), // Dubai
  Aeropuerto("SVO", 12500, 12500, 300) // Moscú
)

val vuelosCurso = List(
  Vuelo("AIRVZLA", 601, "MID", 5, 0, "SVCS", 6, 0, 0),
  Vuelo("AIRVZLA", 602, "SVCS", 6, 30, "MID", 7, 30, 0),
  Vuelo("AVA", 9432, "CLO", 7, 0, "SVO", 2, 20, 4),
  Vuelo("AVA", 9432, "CLO", 7, 0, "BOG", 8, 0, 0),
  Vuelo("IBERIA", 505, "BOG", 18, 0, "MAD", 12, 0, 0),
  Vuelo("IBERIA", 506, "MAD", 14, 0, "SVO", 23, 20, 0),
  Vuelo("IBERIA", 507, "MAD", 16, 0, "SVO", 1, 20, 0),
  Vuelo("LATAM", 787, "BOG", 17, 0, "MEX", 19, 0, 0),
  Vuelo("VIVA", 756, "BOG", 9, 0, "MDE", 10, 0, 0),
  Vuelo("VIVA", 769, "MDE", 11, 0, "BAQ", 12, 0, 0),
  Vuelo("AVA", 5643, "BAQ", 14, 0, "MEX", 16, 0, 0),
  Vuelo("COPA", 1234, "CTG", 10, 0, "PTY", 11, 30, 0),
  Vuelo("AVA", 4321, "CTG", 9, 30, "SMR", 10, 0, 0),
  Vuelo("COPA", 7631, "SMR", 10, 50, "PTY", 11, 50, 0),
  Vuelo("TURKISH", 7799, "CLO", 7, 0, "IST", 14, 0, 3),
  Vuelo("QATAR", 5566, "IST", 23, 0, "SVO", 2, 0, 0)
)

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
  val aeropuertosMap = aeropuertos.map(airport => airport.cod -> airport).toMap

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
  val aeropuertosMap = aeropuertos.map(airport => airport.cod -> airport).toMap

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







