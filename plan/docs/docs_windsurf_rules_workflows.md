Rules
Users can explicitly define their own rules for Cascade to follow.

Rules can be defined at either the global level or the workspace level.

global_rules.md - rules applied across all workspaces

.windsurf/rules - workspace level repo containing rules that are tied to globs or natural language descriptions.

To get started with Rules, click on the Customizations icon in the top right slider menu in Cascade, then navigate to the Rules panel. Here, you can click on the + Global or + Workspace button to create new rules at either the global or workspace level, respectively.

You can find example rule templates curated by the Windsurf team at https://windsurf.com/editor/directory to help you get started.
Rules files are limited to 6000 characters each. Any content above 6000 characters will be truncated and Cascade will not be aware of them.

If the total of your global rules and local rules exceed 12,000 characters, priority will be given to the global rules, followed by the workspace rules. Any rules beyond 12,000 characters will be truncated.

​
Activation Modes
At the rule level, you can define how a rule should be activated for Cascade.

There are 4 modes:

Manual: This rule can be manually activated via @mention in Cascade’s input box
Always On: This rule will always be applied
Model Decision: Based on a natural language description of the rule the user defines, the model decides whether to apply the rule.
Glob: Based on the glob pattern that the user defines (e.g. .js, src/**/.ts), this rule will be applied to all files that match the pattern.
​
Best Practices
To help Cascade follow your rules effectively, follow these best practices:

Keep rules simple, concise, and specific. Rules that are too long or vague may confuse Cascade.
There’s no need to add generic rules (e.g. “write good code”), as these are already baked into Cascade’s training data.
Format your rules using bullet points, numbered lists, and markdown. These are easier for Cascade to follow compared to a long paragraph. For example:

# Coding Guidelines 
- My project's programming language is python
- Use early returns when possible
- Always add documentation when creating new functions and classes
XML tags can be an effective way to communicate and group similar rules together. For example:


<coding_guidelines>
- My project's programming language is python
- Use early returns when possible
- Always add documentation when creating new functions and classes
</coding_guidelines>

Workflows
Workflows enable users to define a series of steps to guide Cascade through a repetitive set of tasks, such as deploying a service or responding to PR comments.

These Workflows are saved as markdown files, allowing users and their teams an easy repeatable way to run key processes.

Once saved, Workflows can be invoked in Cascade via a slash command with the format of /[name-of-workflow]

​
How it works
Rules generally provide large language models with guidance by providing persistent, reusable context at the prompt level.

Workflows extend this concept by providing a structured sequence of steps or prompts at the trajectory level, guiding the model through a series of interconnected tasks or actions.


To execute a workflow, users simply invoke it in Cascade using the /[workflow-name] command.

Upon invocation, Cascade sequentially processes each step defined in the workflow, performing actions or generating responses as specified.

​
How to create a Workflow
To get started with Workflows, click on the Customizations icon in the top right slider menu in Cascade, then navigate to the Workflows panel. Here, you can click on the + Workflow button to create a new Workflow.

Workflows are saved as markdown files within the repository root of .windsurf/workflows/ and contains a title, description, and a series of steps with specific instructions for Cascade to follow.

​
Generate a Workflow with Cascade
You can also ask Cascade to generate Workflows for you! This works particularly well for workflows involving a series of steps in a particular CLI tool.