package com.github.acout.scalaretro

import scala.meta._
import java.nio.file.Path
import java.io.{File, FileWriter}

object Main extends App{
    
    def getAllScalaFiles(p: Path): List[File] = {
        val d = p.toFile
        if (!d.exists || !d.isDirectory) {
            List[File]()
        } else {
            d.listFiles.filter(_.isFile).toList ++ d.listFiles.filter(_.isDirectory).flatMap(d => getAllScalaFiles(d.toPath))
        }
    }

    val path = java.nio.file.Paths.get("..", "unsupervise", "c4e-arch1", "core", "src", "main", "scala")
    //val path = java.nio.file.Paths.get("src", "main", "resources")
    val files = getAllScalaFiles(path)

    val tokens = new ScalaTokenizer().tokenize(files)
    new MermaidClassDiagramWriter(new FileWriter(new File("output.md"))).write(tokens)

}