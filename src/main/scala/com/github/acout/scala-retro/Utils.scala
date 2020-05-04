package com.github.acout.scalaretro

import java.nio.file.Path
import java.io.File

object Utils {
    def getAllScalaFiles(p: Path): List[File] = {
        val d = p.toFile
        if (!d.exists || !d.isDirectory) {
            List[File]()
        } else {
            d.listFiles.filter(_.isFile).toList ++ d.listFiles.filter(_.isDirectory).flatMap(d => getAllScalaFiles(d.toPath))
        }
    }
}