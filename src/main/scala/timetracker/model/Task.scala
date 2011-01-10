package com.kolesky.timetracker.model

// LERNIN: Should this be a case class?
class Task(val day: Day, val duration: Int, val rawCategory: String, val description: String) {
  val repeat = rawCategory.endsWith("*")
  val category = if (repeat) { rawCategory.dropRight(1) } else { rawCategory }

  override def toString = {
    format("   Task: %s; %s; %s", duration, category, description)
  }
}
