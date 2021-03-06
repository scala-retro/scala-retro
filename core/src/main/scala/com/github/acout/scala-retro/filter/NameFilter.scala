package com.github.acout.scalaretro.core.filter

import com.github.acout.scalaretro.core.token._

object NameFilter{

    def apply(regex: String)(t: Token): Boolean = {
        t match {
            case ClassToken(name, _, _, _, _) => name.matches(regex)
            case InheritanceToken(from, to) => from.matches(regex) && to.matches(regex)
            case AssociationToken(from, to) => from.matches(regex) && to.matches(regex)
            case DependencyToken(from, to) => from.matches(regex) && to.matches(regex)
        }
    }

}