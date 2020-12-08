package com.github.acout.scalaretro.core.tokenizer

import com.github.acout.scalaretro.core.writer._

object TestPkg extends App {
  import com.github.acout.scalaretro.core.tokenizer.ScalaTokenizer
  import com.github.acout.scalaretro.core.writer.MermaidClassDiagramWriter
  import com.github.acout.scalaretro.core.Utils
  import java.io.{File, FileWriter}

  //Retrieve all ".scala" files recursively from one root folder
  val files = Utils.getAllScalaFiles(new File("D:\\scala-retro\\core\\src\\main\\scala").toPath)
  //Tokenize the different files as a flatMapped list of Token
  val tokens = new ScalaTokenizer().tokenize(files)
  //println(tokens.mkString("\n"))
  new MermaidClassDiagramHTMLWriter(new FileWriter("D:\\output.html")) {
    write(tokens)
    close
  }
}
