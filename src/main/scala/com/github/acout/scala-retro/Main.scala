package com.github.acout.scalaretro

import java.io.{File, FileWriter}

object Main extends App{

    val files = Utils.getAllScalaFiles(new File(args.head).toPath)
    val tokens = new ScalaTokenizer().tokenize(files)
    val writer = new MermaidClassDiagramWriter(new FileWriter(new File(args(1))))
    writer.write(tokens)
    writer.close

}