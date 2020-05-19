package com.github.acout.scalaretro.core.tokenizer

import com.github.acout.scalaretro.core.token.Token

import java.io.File
import java.nio.file.Path

trait Tokenizer {

    def tokenize(f: File): List[Token]
    def tokenize(p: Path): List[Token] = tokenize(p.toFile)

}