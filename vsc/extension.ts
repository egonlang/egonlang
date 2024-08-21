"use strict";

import {
  workspace,
  ExtensionContext,
  commands,
  tasks,
  window,
  env,
  Uri,
} from "vscode";
import { EgonTaskProvider } from "./egonTaskProvider";

export function activate(context: ExtensionContext) {
  const binPath: string =
    workspace.getConfiguration("egon").get("binPath") ?? "egon";

  const lexFileHandler = () => {
    const terminal = window.createTerminal(`egon lex`);
    terminal.sendText(
      `${binPath} lex ${window.activeTextEditor.document.uri.path}`
    );
    terminal.show();
  };

  const lexFileToFileHandler = async () => {
    const filename = await window.showInputBox({
      placeHolder: "File",
      prompt: "Where to write the lex json to?",
      value: "temp-lex.json"
    });

    const terminal = window.createTerminal(`egon lex ${filename}`);

    terminal.sendText(
      `${binPath} lex ${window.activeTextEditor.document.uri.path} > ${filename} && exit`
    );
    terminal.show(true);
  };

  const parseFileHandler = () => {
    const terminal = window.createTerminal(`egon parse`);
    terminal.sendText(
      `${binPath} parse ${window.activeTextEditor.document.uri.path}`
    );
    terminal.show();
  };

  const parseFileToFileHandler = async () => {
    const filename = await window.showInputBox({
      placeHolder: "File",
      prompt: "Where to write the parse json to?",
      value: "temp-parse.json"
    });

    if (!filename) {
      return;
    }

    const terminal = window.createTerminal(`egon parse ${filename}`);

    terminal.sendText(
      `${binPath} parse ${window.activeTextEditor.document.uri.path} > ${filename} && exit`
    );
    terminal.show(true);
  };

  context.subscriptions.push(
    commands.registerCommand("egon.lexCurrentFile", lexFileHandler),
    commands.registerCommand("egon.lexCurrentFileToFile", lexFileToFileHandler),
    commands.registerCommand("egon.parseCurrentFile", parseFileHandler),
    commands.registerCommand("egon.parseCurrentFileToFile", parseFileToFileHandler),
    commands.registerCommand("egon.openGithub", () => {
      env.openExternal(Uri.parse("https://github.com/egonlang/egonlang"));
    }),
  );

  tasks.registerTaskProvider("egon", new EgonTaskProvider());
}

export function deactivate() { }
