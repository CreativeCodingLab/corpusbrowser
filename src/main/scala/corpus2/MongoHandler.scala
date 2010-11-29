package corpus2

import com.mongodb._
import com.osinka.mongodb._
import com.osinka.mongodb.shape._
import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.parsing.combinator._
import scala.util.matching.Regex
import scala.xml._
//import scala.collection._
import scala.collection.mutable._
import scala.util.control.Breaks._
//import scala.tools.nsc.io._
import java.io.File

case class User(val name: String) extends MongoObject

object User extends MongoObjectShape[User] {
  lazy val name = Field.scalar("name", _.name)
  override lazy val * = List(name)
  override def factory(dbo: DBObject): Option[User] =
    for { name(n) ← Some(dbo) } yield new User(n)
}

object MongoHandler {
  val mongo = new Mongo();

  def testMongo(): Unit = {

    println("Found databases: ")
    mongo.getDatabaseNames().foreach(println)
    val db = mongo.getDB("corpus");
    println("the corpus database has the following collections: " + db.getCollectionNames())
    println("done testing...")
  }

  def saveToMongo(sentences: ListBuffer[Sentence]): Unit = {
    for (s ← mongo.getDatabaseNames()) {
      println(s);
    }

    val db = mongo.getDB("corpus");

    val dbColl = db.getCollection("sentences");

    //val dbColl: DBCollection

    for (s ← sentences) {
      //val doc = new BasicDBObject();
      val doc = new User("Abigail");

      //doc.put("name", "JoeJoe2");

      //dbColl.insert(doc);
      //dbColl << (doc);

      // val coll = new DBObjectCollection(dbColl);

      // for (obj <- coll)
      // {
      //	println("in foo... " + obj)
      // }

      //val scol = colB of User;
      //	col foreach {

      //u => println(u) }

    }
  }

}
