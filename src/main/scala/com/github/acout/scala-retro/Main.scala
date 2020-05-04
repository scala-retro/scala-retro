package com.github.acout.scalaretro

import java.io.{File, FileWriter}

object Main extends App{

    val regex = if(args.length > 2) args(2) else ".*"

    val files = Utils.getAllScalaFiles(new File(args.head).toPath)
    val tokens = new ScalaTokenizer().tokenize(files)
    val writer = new MermaidClassDiagramWriter(new FileWriter(new File(args(1))))
    writer.write(tokens.filter(t => {
        t match {
            case ClassToken(name, _, _) => name.matches(regex)
            case InheritanceToken(from, to) => from.matches(regex) && to.matches(regex)
            case AssociationToken(from, to) => from.matches(regex) && to.matches(regex)
        }
    }))
    writer.close

}