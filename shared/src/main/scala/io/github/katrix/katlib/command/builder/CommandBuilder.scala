/*
 * This file is part of KatLib, licensed under the MIT License (MIT).
 *
 * Copyright (c) 2016 Katrix
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package io.github.katrix.katlib.command.builder

import scala.annotation.tailrec

import org.spongepowered.api.Sponge
import org.spongepowered.api.command.args.{CommandElement, GenericArguments}
import org.spongepowered.api.command.spec.{CommandExecutor, CommandSpec}
import org.spongepowered.api.command.{CommandException, CommandResult, CommandSource}
import org.spongepowered.api.text.Text

import io.github.katrix.katlib.KatPlugin
import io.github.katrix.katlib.helper.Implicits._

/**
	* A CommandBuilder to easily and type safe create commands.
	*/
sealed case class CommandBuilder[A] private(
	name: String,
	commandSpec: CommandSpec.Builder = CommandSpec.builder(),
	parameters: Seq[(Text, CommandElement)] = Seq(),
	executor: Option[A => CommandResult] = None) {

	/**
		*
		*/
	def withDescription(description: Text): CommandBuilder[A] = copy(commandSpec = commandSpec.description(description))

	/**
		* Sets the permission required.
		*/
	def withPermission(permission: String): CommandBuilder[A] = copy(commandSpec = commandSpec.permission(permission))

	/**
		* Adds a parameter to the command.
		*
		* @param parameterType The parameter object.
		* The string used in the usage will be pulled from the [[Parameters.ParameterType.toString]] method of the parameter object.
		*/
	def takes[B](parameterType: Parameters.ParameterType[B]): CommandBuilder[(A, B)] = copy(
		parameters = parameters :+ (parameterType.name, parameterType.element), executor = None,
		commandSpec = commandSpec.arguments(parameterType.element +: parameters.map(_._2): _*))

	/**
		* Sets the executor for this command.
		* Use pattern matching to get hold of all the parameters.
		*/
	def executes(executor: A => CommandResult): CommandBuilder[A] = copy(executor = Some(executor))

	/**
		* Registers this command as a top level command.
		*/
	def register(implicit plugin: KatPlugin): CommandBuilder[A] = {
		executor match {
			case Some(ex) =>
				val commandExecutor: CommandExecutor = (src, ctx) => {

					@tailrec
					def convertArgs(remaining: Seq[Text], convertedParams: Seq[Option[Any]]): Seq[Option[Any]] = {
						if(remaining == Nil) convertedParams
						else {
							val currentParam = remaining.head
							val convertedParam = ctx.getOne[Any](currentParam).toOption
							convertArgs(remaining.tail, convertedParams :+ convertedParam)
						}
					}

					val convertedArgs = convertArgs(parameters.map(_._1), Seq())
					val flattenedArgs = src +: convertedArgs.flatten
					if(flattenedArgs.size - 1 != convertedArgs.size) throw new CommandException("Missing argument".text)
					val executorArgs = if(flattenedArgs.size == 1) flattenedArgs.head
					else flattenedArgs.drop(2).fold((flattenedArgs.head, flattenedArgs(1)))((t, e) => (t, e))
					ex(executorArgs.asInstanceOf[A])
				}
				val newBuilder = copy(commandSpec = commandSpec.executor(commandExecutor))

				Sponge.getCommandManager.register(plugin, newBuilder.commandSpec.build(), name)
				newBuilder
			case None => throw new IllegalStateException()
		}
	}
}

object CommandBuilder {

	//example
	val test = command("test") takes new Parameters.StringArg() executes { case ((src, string)) =>

		CommandResult.success()
	}
	/**
		* Creates a new command with the specified name.
		*/
	def command(name: String): CommandBuilder[CommandSource] = CommandBuilder(name)
}

object Parameters {

	case class ParameterType[A](name: Text, element: CommandElement) {
		override def toString: String = name.toString
	}

	class StringArg(name: Text = "string".text) extends ParameterType[String](name, GenericArguments.string(name))
	class DoubleArg(name: Text = "double".text) extends ParameterType[Double](name, GenericArguments.doubleNum(name))
}
