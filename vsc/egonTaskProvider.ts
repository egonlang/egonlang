import * as vscode from "vscode";

export class EgonTaskProvider implements vscode.TaskProvider {
  static TaskType = "egon";
  private task: vscode.Task | undefined;

  constructor() {
    this.task = new vscode.Task(
      {
        type: EgonTaskProvider.TaskType,
      },
      vscode.TaskScope.Workspace,
      "run",
      "egon"
    );
  }

  async provideTasks(token: vscode.CancellationToken): Promise<vscode.Task[]> {
    const tasks = [];
    tasks.push(this.task);
    return tasks;
  }

  resolveTask(
    task: vscode.Task,
    _token: vscode.CancellationToken
  ): vscode.Task | undefined {
    return new vscode.Task(
      task.definition,
      vscode.TaskScope.Workspace,
      task.name,
      task.source,
      new vscode.ShellExecution(
        `egon parse ${this.getCurrentOpenFilePath()}`,
        {}
      )
    );
  }

  private getCurrentOpenFilePath(): string {
    return vscode.window.activeTextEditor.document.uri.path;
  }
}
