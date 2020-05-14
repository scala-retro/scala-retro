package com.github.acout.scalaretro.core.token

sealed trait Encapsulation
case object Private extends Encapsulation
case object Public extends Encapsulation
case object Protected extends Encapsulation

case class Parameter(name: String, t: String)
case class Attribute(name: String, t: String, encapsulation: Encapsulation)
case class Method(name: String, params: List[List[Parameter]], returnType: String)

sealed trait Token
case class ClassToken(name: String, attributes: List[Attribute], methods: List[Method]) extends Token
case class InheritanceToken(child: String, parent: String) extends Token
case class AssociationToken(source: String, target: String) extends Token
case class DependencyToken(source: String, target: String) extends Token

/*case class Tokens(tokens: List[Token]){
    def ++(otherTokens: Tokens) = Tokens(tokens ++ otherTokens.tokens)
    def filter(fun: Token => Boolean) = tokens.filter(fun)
}*/