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
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";
import { EgonTaskProvider } from "./egonTaskProvider";

let lc: LanguageClient;

export function activate(context: ExtensionContext) {
  const binPath: string =
    workspace.getConfiguration("egon").get("binPath") ?? "egon";

  const lspPath = `${binPath}lang-lsp`;
  const serverOptions: ServerOptions = {
    command: lspPath
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      {
        language: "egon",
      },
    ],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/*.eg"),
    },
    outputChannelName: "egon",
  };

  lc = new LanguageClient(
    "egon-language-server",
    serverOptions,
    clientOptions
  );

  const startLanguageServerHandler = () => {
    console.log("Starting egon language server...");
    return lc.start();
  };

  const stopLanguageServerHandler = () => {
    console.log("Stopping egon language server...");

    if (!lc) {
      return undefined;
    }

    return lc.stop();
  };

  const restartLanguageServerHandler = async () => {
    console.log("Restarting egon language server...");

    await stopLanguageServerHandler();

    await startLanguageServerHandler();
  };

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
    commands.registerCommand(
      "egon.startLanguageServer",
      startLanguageServerHandler
    ),
    commands.registerCommand(
      "egon.stopLanguageServer",
      stopLanguageServerHandler
    ),
    commands.registerCommand(
      "egon.restartLanguageServer",
      restartLanguageServerHandler
    ),
    commands.registerCommand("egon.openGithub", () => {
      env.openExternal(Uri.parse("https://github.com/egonlang/egonlang"));
    }),
  );

  tasks.registerTaskProvider("egon", new EgonTaskProvider());

  lc.start();
}

export function deactivate() { }
