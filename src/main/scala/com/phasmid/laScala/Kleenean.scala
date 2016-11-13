package com.phasmid.laScala

import com.phasmid.laScala.fp.FP

trait OptBool {
  def value: Option[Boolean]
  def :&(x: Option[Boolean]): OptBool = Kleenean(FP.map2(x,value)(_ && _) orElse Kleenean.&&&(x) orElse Kleenean.&&&(value))
  def :|(x: Option[Boolean]): OptBool = Kleenean(FP.map2(x,value)(_ || _) orElse Kleenean.|||(x) orElse Kleenean.|||(value))
  def &:(x: Option[Boolean]): OptBool = Kleenean(FP.map2(value,x)(_ && _) orElse Kleenean.&&&(x) orElse Kleenean.&&&(value))
  def |:(x: Option[Boolean]): OptBool = Kleenean(FP.map2(value,x)(_ || _) orElse Kleenean.|||(x) orElse Kleenean.|||(value))
  def :&(y: OptBool): OptBool = Kleenean(FP.map2(value,y.value)(_ && _) orElse Kleenean.&&&(y.value) orElse Kleenean.&&&(value))
  def :|(y: OptBool): OptBool = Kleenean(FP.map2(value,y.value)(_ || _) orElse Kleenean.|||(y.value) orElse Kleenean.|||(value))
  def :&(b: Boolean): OptBool = Kleenean(FP.map2(Some(b),value)(_ && _) orElse Kleenean.&&&(Some(b)))
  def :|(b: Boolean): OptBool = Kleenean(FP.map2(Some(b),value)(_ || _) orElse Kleenean.|||(Some(b)))
  def &:(b: Boolean): OptBool = Kleenean(FP.map2(value,Some(b))(_ && _) orElse Kleenean.&&&(Some(b)))
  def |:(b: Boolean): OptBool = Kleenean(FP.map2(value,Some(b))(_ || _) orElse Kleenean.|||(Some(b)))
}

case class Kleenean(value: Option[Boolean]) extends OptBool

case object ^^ extends OptBool {
  def value = None
}

object Kleenean {
  def apply(x: Boolean): OptBool = Kleenean(Some(x))
  def apply(): OptBool = Kleenean(None)
  def &&&(to: Option[Boolean]): Option[Boolean] = to match {
    case Some(true) => None
    case Some(false) => Some(false)
    case None => None
  }
  def |||(to: Option[Boolean]): Option[Boolean] = to match {
    case Some(false) => None
    case Some(true) => Some(true)
    case None => None
  }
}