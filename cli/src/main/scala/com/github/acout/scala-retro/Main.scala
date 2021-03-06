package com.github.acout.scalaretro.cli

import com.github.acout.scalaretro.core.tokenizer.ScalaTokenizer
import com.github.acout.scalaretro.core.writer.MermaidClassDiagramWriter
import com.github.acout.scalaretro.core.filter.NameFilter
import com.github.acout.scalaretro.core.Utils

import java.io.{File, FileWriter}

object Main extends App{

    val regex = if(args.length > 2) args(2) else ".*"

    val files = Utils.getAllScalaFiles(new File(args.head).toPath)
    val tokens = new ScalaTokenizer().tokenize(files)
    val writer = new MermaidClassDiagramWriter(new FileWriter(new File(args(1))))
    val nameFilter = NameFilter(regex) _
    writer.write(tokens.filter(nameFilter))
    writer.close

}