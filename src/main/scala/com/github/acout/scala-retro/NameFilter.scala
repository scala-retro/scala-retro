package com.github.acout.scalaretro

object NameFilter{

    def apply(regex: String)(t: Token): Boolean = {
        t match {
            case ClassToken(name, _, _) => name.matches(regex)
            case InheritanceToken(from, to) => from.matches(regex) && to.matches(regex)
            case AssociationToken(from, to) => from.matches(regex) && to.matches(regex)
            case DependencyToken(from, to) => from.matches(regex) && to.matches(regex)
        }
    }

}