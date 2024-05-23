package object Itinerarios {
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
    def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
        val aeropuertosMap =
        aeropuertos.map(airport => airport.cod -> airport).toMap
        
        def formarItinerarios(cod1: String,cod2: String,visitados: Set[String]): List[Itinerario] = {
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


    def itinerariosEscalas(vuelos:List[Vuelo],aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
        val itinerariosFunc = itinerarios(vuelos, aeropuertos)

        (origen: String, destino: String) => {
            val todosItinerarios = itinerariosFunc(origen, destino)

            // Función para calcular el número total de escalas en un itinerario
            def numeroDeEscalas(itinerario: Itinerario): Int = {
            itinerario.sliding(2).map {
                case List(vuelo1, vuelo2) => vuelo1.Esc + (if (vuelo1.Dst != vuelo2.Org) 1 else 0)
                case _ => 0
              }.sum
            }

            // Encontrar el número mínimo de escalas en todos los itinerarios
            val minNumEscalas = todosItinerarios.map(numeroDeEscalas).min

            // Filtrar los itinerarios con el menor número de escalas
            todosItinerarios.filter(itinerario => numeroDeEscalas(itinerario) == minNumEscalas)
        }
    }
}
