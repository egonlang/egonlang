"use strict";

import {
  workspace,
  ExtensionContext,
  commands,
  tasks,
  window,
  env,
  Uri,
  languages
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


  const parseFileHandler = () => {
    const terminal = window.createTerminal(`egon parse`);
    terminal.sendText(
      `${binPath} parse ${window.activeTextEditor.document.uri.path}`
    );
    terminal.show();
  };

  context.subscriptions.push(
    commands.registerCommand("egon.lexCurrentFile", lexFileHandler),
    commands.registerCommand("egon.parseCurrentFile", parseFileHandler),
    commands.registerCommand("egon.openGithub", () => {
      env.openExternal(Uri.parse("https://github.com/egonlang/egonlang"));
    }),
  );

  tasks.registerTaskProvider("egon", new EgonTaskProvider());
}

export function deactivate() { }
