import sbt._
import de.element34.sbteclipsify._
//import com.github.olim7t.sbtscalariform._

class MongoProject(info: ProjectInfo) extends DefaultProject(info) 
  //with Eclipsify 
  //with ScalariformPlugin 
  {

    //override def jvmArgs = "-Xms512m -Xmx2g"
    //override def compileOptions = ExplainTypes :: CompileOption("jvmargs='-Xmx1024m'") :: super.compileOptions.toList
  
    val mongo = "com.osinka" % "mongo-scala-driver_2.8.0" % "0.8.2"

  //val vsdRels = "vsd-releases" at "http://scala-tools.org/repo-releases"
  val vsd = "org.scala-tools" % "vscaladoc" % "1.1"

  val bumRels = "bum-releases" at "http://repo.bumnetworks.com/releases"
  val bumSnaps = "bum-snapshots" at "http://repo.bumnetworks.com/snapshots"
  val casbah = "com.novus" % "casbah_2.8.0" % "1.0.8.5"

  //override def scalariformOptions = Seq(VerboseScalariform)
}
