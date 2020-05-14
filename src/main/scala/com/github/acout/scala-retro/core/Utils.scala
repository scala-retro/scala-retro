package com.github.acout.scalaretro.core

import java.nio.file.Path
import java.io.File

object Utils {
    def getAllScalaFiles(p: Path): List[File] = {
        def getAllScalaFilesRecursively(d: File): List[File] = {
            if (!d.exists || !d.isDirectory) {
                List[File]()
            } else {
                d.listFiles.filter(_.isFile).toList ++ d.listFiles.filter(_.isDirectory).flatMap(d => getAllScalaFiles(d.toPath))
            }
        }
        val f = p.toFile
        if (!f.exists) {
            List[File]()
        } else {
            if(f.isDirectory){
                getAllScalaFilesRecursively(f)
            }else{
                List(f)
            }
        }
    }
}