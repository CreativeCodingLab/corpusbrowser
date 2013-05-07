import sbt._

class MySbtProjectPlugins(info: ProjectInfo) extends PluginDefinition(info) {

  val scalatoolsSnapshot = "Scala Tools Snapshot" at "http://scala-tools.org/repo-snapshots/"
  //val formatter = "com.github.olim7t" % "sbt-scalariform" % "1.0.1"

  lazy val eclipse = "de.element34" % "sbt-eclipsify" % "0.6.0"
}
