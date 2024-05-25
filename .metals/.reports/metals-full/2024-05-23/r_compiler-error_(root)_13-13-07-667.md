file://<HOME>/Desktop/Univalle/Cuarto%20Semestre/Programacion%20funcional%20y%20concurrente/proyectoFinal/GestioVuelos/src/main/scala/Itinerarios/package.scala
### java.lang.IndexOutOfBoundsException: -1 is out of bounds (min 0, max 2)

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 2.13.12
Classpath:
<WORKSPACE>/.bloop/root/bloop-bsp-clients-classes/classes-Metals-c6Q1pWNZS9qlB_u-KM2oVA== [exists ], <HOME>/Library/Caches/bloop/semanticdb/com.sourcegraph.semanticdb-javac.0.9.10/semanticdb-javac-0.9.10.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.12/scala-library-2.13.12.jar [exists ]
Options:
-Yrangepos -Xplugin-require:semanticdb


action parameters:
uri: file://<HOME>/Desktop/Univalle/Cuarto%20Semestre/Programacion%20funcional%20y%20concurrente/proyectoFinal/GestioVuelos/src/main/scala/Itinerarios/package.scala
text:
```scala
import common._
import scala.util.Random
import scala.collection.parallel.immutable.ParVector
import Benchmark._
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
  def itinerarios(
      vuelos: List[Vuelo],
      aeropuertos: List[Aeropuerto]
  ): (String, String) => List[Itinerario] = {
    val aeropuertosMap =
      aeropuertos.map(airport => airport.cod -> airport).toMap
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
    
    
    def tiempoItinerario( itinerario: List[Vuelo] ): (Int) = 
    {

      val aeropuertosMap =aeropuertos.map(airport => airport.cod -> airport).toMap
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

  def itinerariosAirePar( vuelos : List[Vuelo] , aeropuertos:List[Aeropuerto] ) : ( String , String ) =>List [ Itinerario ]= {
      // Recibe vuelos , una lista de todos los vuelos disponibles y
      // aeropuertos una lista de todos los aeropuertos
      // y devuelve una funcion que recibe c1 y c2 , codigos de aeropuertos
      // y devuelve los tres ( si los hay ) itinerarios que minimizan el tiempo en el aire entre esos dos aeropuertos

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
      
      
      def tiempoItinerario( itinerario: List[Vuelo] ): (Int) = 
      {

        val aeropuertosMap =aeropuertos.map(airport => airport.cod -> airport).toMap
        val tiempoVuelos = itinerarioPa.map( vuelo => task{
            val GMTOrg = aeropuertosMap(vuelo.Org).GMT
            val GMTDst = aeropuertosMap(vuelo.Dst).GMT
            val (horaSalida, horaLLegada) = parallel(convertirHorasGMT(vuelo.HS, vuelo.MS , GMTOrg),convertirHorasGMT(vuelo.HL, vuelo.ML , GMTDst))
            sumarHoras(horaSalida._1 , horaSalida._2 , horaLLegada._1 , horaLLegada._2 , '-')
        })
        tiempoVuelos.map( t => ( (t._1)*60) + t._2 ).sum
      }
      (c1: String , c2: String)=>{
        val itinerarioOpc = itinerarios( vuelos, aeropuertos)(c1,c2)
        val tiemposTotales = itinerarioOpc.map( it => 
          (tiempoItinerario(it), it)
        ).sortBy(_._1).take(3)
      }
  }
  

}







```



#### Error stacktrace:

```
scala.collection.mutable.ArrayBuffer.apply(ArrayBuffer.scala:106)
	scala.reflect.internal.Types$Type.findMemberInternal$1(Types.scala:1030)
	scala.reflect.internal.Types$Type.findMember(Types.scala:1035)
	scala.reflect.internal.Types$Type.memberBasedOnName(Types.scala:661)
	scala.reflect.internal.Types$Type.member(Types.scala:625)
	scala.tools.nsc.typechecker.Typers$Typer.member(Typers.scala:668)
	scala.tools.nsc.typechecker.Typers$Typer.$anonfun$typed1$57(Typers.scala:5352)
	scala.tools.nsc.typechecker.Typers$Typer.typedSelect$1(Typers.scala:5352)
	scala.tools.nsc.typechecker.Typers$Typer.typedSelectOrSuperCall$1(Typers.scala:5502)
	scala.tools.nsc.typechecker.Typers$Typer.typed1(Typers.scala:6098)
	scala.tools.nsc.typechecker.Typers$Typer.typed(Typers.scala:6153)
	scala.tools.nsc.typechecker.Typers$Typer.$anonfun$typed1$41(Typers.scala:5160)
	scala.tools.nsc.typechecker.Typers$Typer.silent(Typers.scala:698)
	scala.tools.nsc.typechecker.Typers$Typer.normalTypedApply$1(Typers.scala:5162)
	scala.tools.nsc.typechecker.Typers$Typer.typedApply$1(Typers.scala:5194)
	scala.tools.nsc.typechecker.Typers$Typer.typed1(Typers.scala:6097)
	scala.tools.nsc.typechecker.Typers$Typer.typed(Typers.scala:6153)
	scala.tools.nsc.typechecker.Typers$Typer.typedStat$1(Typers.scala:6231)
	scala.tools.nsc.typechecker.Typers$Typer.$anonfun$typedStats$8(Typers.scala:3470)
	scala.tools.nsc.typechecker.Typers$Typer.typedStats(Typers.scala:3470)
	scala.tools.nsc.typechecker.Typers$Typer.typedBlock(Typers.scala:2597)
	scala.tools.nsc.typechecker.Typers$Typer.typedOutsidePatternMode$1(Typers.scala:6071)
	scala.tools.nsc.typechecker.Typers$Typer.typed1(Typers.scala:6107)
	scala.tools.nsc.typechecker.Typers$Typer.typed(Typers.scala:6153)
	scala.tools.nsc.typechecker.Typers$Typer.typedDefDef(Typers.scala:6229)
	scala.tools.nsc.typechecker.Typers$Typer.typed1(Typers.scala:6059)
	scala.tools.nsc.typechecker.Typers$Typer.typed(Typers.scala:6153)
	scala.tools.nsc.typechecker.Typers$Typer.typedStat$1(Typers.scala:6231)
	scala.tools.nsc.typechecker.Typers$Typer.$anonfun$typedStats$8(Typers.scala:3470)
	scala.tools.nsc.typechecker.Typers$Typer.typedStats(Typers.scala:3470)
	scala.tools.nsc.typechecker.Typers$Typer.typedTemplate(Typers.scala:2089)
	scala.tools.nsc.typechecker.Typers$Typer.typedClassDef(Typers.scala:1927)
	scala.tools.nsc.typechecker.Typers$Typer.typed1(Typers.scala:6060)
	scala.tools.nsc.typechecker.Typers$Typer.typed(Typers.scala:6153)
	scala.tools.nsc.typechecker.Typers$Typer.typedStat$1(Typers.scala:6231)
	scala.tools.nsc.typechecker.Typers$Typer.$anonfun$typedStats$8(Typers.scala:3470)
	scala.tools.nsc.typechecker.Typers$Typer.typedStats(Typers.scala:3470)
	scala.tools.nsc.typechecker.Typers$Typer.typedTemplate(Typers.scala:2089)
	scala.tools.nsc.typechecker.Typers$Typer.typedModuleDef(Typers.scala:1965)
	scala.tools.nsc.typechecker.Typers$Typer.typed1(Typers.scala:6061)
	scala.tools.nsc.typechecker.Typers$Typer.typed(Typers.scala:6153)
	scala.tools.nsc.typechecker.Typers$Typer.typedStat$1(Typers.scala:6231)
	scala.tools.nsc.typechecker.Typers$Typer.$anonfun$typedStats$8(Typers.scala:3470)
	scala.tools.nsc.typechecker.Typers$Typer.typedStats(Typers.scala:3470)
	scala.tools.nsc.typechecker.Typers$Typer.typedPackageDef$1(Typers.scala:5743)
	scala.tools.nsc.typechecker.Typers$Typer.typed1(Typers.scala:6063)
	scala.tools.nsc.typechecker.Typers$Typer.typed(Typers.scala:6153)
	scala.tools.nsc.typechecker.Typers$Typer.typedStat$1(Typers.scala:6231)
	scala.tools.nsc.typechecker.Typers$Typer.$anonfun$typedStats$8(Typers.scala:3470)
	scala.tools.nsc.typechecker.Typers$Typer.typedStats(Typers.scala:3470)
	scala.tools.nsc.typechecker.Typers$Typer.typedPackageDef$1(Typers.scala:5743)
	scala.tools.nsc.typechecker.Typers$Typer.typed1(Typers.scala:6063)
	scala.tools.nsc.typechecker.Typers$Typer.typed(Typers.scala:6153)
	scala.tools.nsc.typechecker.Analyzer$typerFactory$TyperPhase.apply(Analyzer.scala:124)
	scala.tools.nsc.Global$GlobalPhase.applyPhase(Global.scala:480)
	scala.tools.nsc.interactive.Global$TyperRun.applyPhase(Global.scala:1370)
	scala.tools.nsc.interactive.Global$TyperRun.typeCheck(Global.scala:1363)
	scala.tools.nsc.interactive.Global.typeCheck(Global.scala:680)
	scala.meta.internal.pc.PcCollector.<init>(PcCollector.scala:29)
	scala.meta.internal.pc.PcSemanticTokensProvider$Collector$.<init>(PcSemanticTokensProvider.scala:19)
	scala.meta.internal.pc.PcSemanticTokensProvider.Collector$lzycompute$1(PcSemanticTokensProvider.scala:19)
	scala.meta.internal.pc.PcSemanticTokensProvider.Collector(PcSemanticTokensProvider.scala:19)
	scala.meta.internal.pc.PcSemanticTokensProvider.provide(PcSemanticTokensProvider.scala:73)
	scala.meta.internal.pc.ScalaPresentationCompiler.$anonfun$semanticTokens$1(ScalaPresentationCompiler.scala:170)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: -1 is out of bounds (min 0, max 2)