# Mermaid format research notes

Date: 2025.12.25

## Core format and structure
- Mermaid diagrams are text based. The first non-frontmatter line declares the diagram type (for example: flowchart, sequenceDiagram, classDiagram).
- Exception: YAML frontmatter may appear before the diagram. The frontmatter is delimited by --- lines.
- Comments are lines that start with %% and run to end of line.
- Unknown words or misspellings can break parsing. Some invalid parameters are ignored silently.
- Diagram output is SVG (and can be rasterized by host tooling).

## Files and embedding
- Common extensions: .mmd and .mermaid (not required by Mermaid, but common in tooling).
- Markdown: ```mermaid fenced code blocks.
- HTML: <pre class="mermaid"> ... </pre> and mermaid.run or mermaid.initialize on load.

## Configuration sources
Mermaid builds the render config from multiple sources (in order of application):
- Default config.
- Site config: set by mermaid.initialize (applies across a site/app).
- Frontmatter config: YAML at top of a diagram (v10.5.0+).
- Directives: %%{init: {...}}%% inside a diagram (deprecated in v10.5.0+).

Frontmatter example:
---
config:
  theme: base
  themeVariables:
    primaryColor: "#00ff00"
---
flowchart
  A --> B

Directive example (deprecated):
%%{init: { "theme": "dark", "flowchart": { "curve": "linear" } }}%%
flowchart LR
  A --> B

## Security and interactivity
- securityLevel values: strict (default), antiscript, loose, sandbox.
  - strict: HTML labels and click interactions disabled.
  - antiscript/loose: HTML labels enabled, click interactions allowed.
  - sandbox: render in iframe (limits interactions).
- Click/links and callbacks rely on non-strict securityLevel.

## Layout and look
- layout options include dagre (default), elk, tidy-tree, cose-bilkent.
- look options include classic and handDrawn (config.look).
- Some diagrams support layout selection via frontmatter (flowchart, state, ER, mindmap via tidy-tree).

## Theming
- Themes: default, neutral, dark, forest, base.
- base is the only modifiable theme; customize with themeVariables.
- Diagram-specific theme variables exist (for example pie, gitGraph, timeline, radar, xychart, quadrantChart).

## Accessibility
- Mermaid inserts aria-roledescription on the SVG.
- accTitle: single-line accessible title.
- accDescr: single-line or block description (accDescr { ... }).

## Math
- Use $$...$$ to render math with KaTeX (supported in some diagrams like flowchart and sequence).
- legacyMathML and forceLegacyMathML control MathML vs KaTeX fallback.

## Icons
- Icon packs can be registered (iconify packs or Font Awesome). Used in flowcharts and architecture diagrams and mindmap icons.
- Flowchart inline icons use fa: prefix, or the icon pack prefix you register.

## Diagram breakers and gotchas
- The word "end" can break flowchart and sequence labels unless wrapped (End, [end], etc).
- Avoid using %%{ }%% in comments; it can be parsed as directives.
- Some diagrams are marked experimental or beta; syntax can change.

## Diagram types and key syntax

### Flowchart / Graph
- Keyword: flowchart or graph. Direction: TB, TD, BT, RL, LR.
- Nodes use bracket shapes: [rect], (round), {diamond}, ((circle)), ((())) double circle, etc.
- Edges: --> (arrow), --- (line), -.-> (dotted), ==> (thick), ~~~ (invisible), --o / --x (circle/cross), labeled edges.
- Subgraphs: subgraph name ... end. Optional direction inside subgraph.
- Text labels: quoted strings for special chars; markdown strings with backticks inside quotes when htmlLabels are off.
- Styling: style, classDef, class, ::: shorthand. linkStyle for edges.
- Interaction: click nodeId callback or URL (requires non-strict security).
- Edge IDs and metadata: e1@--> and e1@{ ... } for animations or curve overrides.
- Expanded node shapes via A@{ shape: rect, label: "..." } (also icon and image shapes).

### Sequence diagram
- Keyword: sequenceDiagram.
- Participants: implicit, or participant/actor declarations; actor types via @{ "type": "boundary" | "control" | "entity" | "database" | "collections" | "queue" }.
- Aliases: participant A as Alice, or alias in @{ "alias": "Alice" }.
- Messages: ->>, -->>, ->, --, -x, --x, -), --) and other variants. Dotted vs solid. Some newer half-arrow variants exist.
- Activations: activate/deactivate or arrow suffix + and -.
- Notes: Note left of/right of/over.
- Control blocks: loop, alt/else, opt, par/and, critical/option, break, rect (background highlight).
- Create/destroy: create participant, destroy participant.
- Autonumber: autonumber to add sequence numbers.
- Actor menus: link or links declarations (URL menus).

### Class diagram
- Keyword: classDiagram.
- Classes: class Name, or via relationships. Members via Name : +field or class Name { ... }.
- Visibility: + public, - private, # protected, ~ package.
- Methods use () and can include return type and classifiers (* abstract, $ static).
- Relationships: <|--, *--, o--, -->, --, ..>, ..|>, .. (labels optional). Cardinality with quoted text.
- Namespaces: namespace Foo { ... }.
- Annotations: <<Interface>>, <<Abstract>>, <<Enumeration>>, etc.
- Notes: note "..." and note for Class "...".
- Direction: direction LR/TB/etc.
- Styling: style, classDef, class, ::: shorthand.

### State diagram
- Keywords: stateDiagram-v2 (new), stateDiagram (legacy).
- States: id, state "Description" as id, or id : Description.
- Transitions: --> with optional labels.
- Start/end: [*] --> state and state --> [*].
- Composite states: state X { ... } with nested [*].
- Choice: state id <<choice>>. Fork/join: <<fork>> and <<join>>.
- Concurrency: -- separators inside composite state.
- Notes: note left/right of state.
- Direction: direction LR/TB etc.
- Styling: classDef + class or ::: (limitations for composite states and start/end noted in docs).

### Entity relationship diagram
- Keyword: erDiagram.
- Relationships: ENTITY ||--o{ ENTITY : label (crow's foot notation). Identifying uses --, non-identifying uses ..
- Cardinality markers: ||, |o, }o, }| and aliases like "one or more".
- Attributes: Entity { type name } with optional PK/FK/UK and "comment".
- Entity labels: Entity[Label].
- Direction: direction LR/TB/etc.
- Styling: style, classDef, ::: shorthand.

### User journey
- Keyword: journey.
- title line optional. Sections: section Name.
- Tasks: Task name: score: Actor1, Actor2 (score 1-5).

### Gantt chart
- Keyword: gantt.
- title, dateFormat, axisFormat, tickInterval, weekday, excludes (dates or weekends).
- Sections: section Name.
- Tasks: Title : [tags], [id], [start], [end or duration]. Tags include active, done, crit, milestone. after and until supported.
- displayMode: compact via frontmatter (displayMode: compact).
- Vertical markers: vert entries.
- Comments: %% line comments.
- Interaction: click taskId call/href (requires non-strict security).

### Pie chart
- Keyword: pie (optional showData), optional title.
- Data: "Label" : value (positive numbers only).
- Config: pie.textPosition; theme variables for colors.

### Quadrant chart
- Keyword: quadrantChart.
- title, x-axis left --> right, y-axis bottom --> top.
- quadrant-1..4 labels.
- Points: Label: [x, y] with values 0..1, optional point styles (color, radius, stroke).
- Config and theme variables available.

### Requirement diagram
- Keyword: requirementDiagram.
- Requirement blocks: requirement/functionalRequirement/interfaceRequirement/performanceRequirement/physicalRequirement/designConstraint.
- Fields: id, text, risk (Low/Medium/High), verifymethod (Analysis/Inspection/Test/Demonstration).
- Element blocks: element name { type: ..., docref: ... }.
- Relationships: source - satisfies -> dest (contains, copies, derives, satisfies, verifies, refines, traces).
- Direction: direction LR/TB/etc.
- Styling: style, classDef, class, ::: shorthand.

### Git graph
- Keyword: gitGraph, optionally with orientation: LR:, TB:, BT:.
- Commands: commit, branch, checkout (or switch), merge, cherry-pick.
- Commit attributes: id, tag, type (NORMAL, REVERSE, HIGHLIGHT).
- Config: showBranches, showCommitLabel, mainBranchName, mainBranchOrder, rotateCommitLabel, parallelCommits.
- Branch ordering: branch name order: N.

### C4 diagram (experimental)
- Keywords: C4Context, C4Container, C4Component, C4Dynamic, C4Deployment.
- PlantUML-compatible syntax for elements (Person, System, Container, Component, Boundary, etc.).
- Relationships: Rel, BiRel, Rel_U/Rel_D/Rel_L/Rel_R, Rel_Back, RelIndex.
- Styling: UpdateElementStyle, UpdateRelStyle. Layout: UpdateLayoutConfig.
- Limitations noted in docs (layout not fully automatic; some features unsupported).

### Mindmap (experimental)
- Keyword: mindmap.
- Indentation defines hierarchy.
- Shapes like flowchart syntax: [rect], (round), ((circle)), etc.
- Icons: ::icon(pack icon) requires icon packs.
- Classes: :::class1 class2.
- Markdown strings inside labels with backticks.

### Timeline (experimental)
- Keyword: timeline.
- title optional; entries use time : event (multiple events per time).
- Sections: section Name for grouping. disableMulticolor to keep same palette.
- Theme variables cScale0..cScale11 control palette.

### Sankey (experimental)
- Keyword: sankey.
- Data lines: source,target,value (CSV-like). Empty lines allowed.
- Quotes required for commas in fields; "" escapes quotes.
- Config: width, height, linkColor (source/target/gradient/hex), nodeAlignment (left/center/right/justify).

### XY chart
- Keyword: xychart (optional horizontal).
- title, x-axis (categories or numeric range), y-axis (numeric range), bar and line series.
- Config: width, height, axis options, showDataLabel; themeVariables for axes and plot colors.

### Block diagram
- Keyword: block.
- columns N to set grid. Blocks can span columns with id:N.
- Nested blocks: block:ID ... end. space and space:N placeholders.
- Shapes similar to flowchart. Block arrows: blockArrowId<['label']>(direction).
- Edges: --> and labeled edges. Styling: style and classDef.

### Packet diagram
- Keyword: packet.
- Fields: start-end: "Label" or +N: "Label" for bit counts. Single-bit uses start: "Label".
- Title optional. Config via packet settings.

### Kanban
- Keyword: kanban.
- Columns: columnId[Title]. Tasks indented beneath column: taskId[Description].
- Task metadata: @{ ticket: ..., assigned: ..., priority: "High" } (priority supports Very High/High/Low/Very Low).
- Config: kanban.ticketBaseUrl for ticket links.

### Architecture diagram (beta)
- Keyword: architecture-beta.
- Groups: group id(icon)[Label] (optional in parent).
- Services: service id(icon)[Label] (optional in group).
- Edges: serviceId:Side -- Side:serviceId with optional arrows and group edges using {group}.
- Junctions: junction id.
- Icons: built-in set (cloud, database, disk, internet, server) plus icon packs.

### Radar diagram (beta)
- Keyword: radar-beta.
- axis lines define axes; curve lines define datasets. Curves can use ordered values or axisId: value pairs.
- Options: title, showLegend, min, max, graticule (circle/polygon), ticks.

### Treemap (beta)
- Keyword: treemap-beta.
- Indentation for hierarchy. Parent nodes: "Label"; leaf nodes: "Label": value.
- Styling: classDef and :::.
- Config: padding, diagramPadding, showValues, valueFormat (D3 format strings).

### ZenUML (external)
- Keyword: zenuml.
- Alternative sequence syntax; participants declared by name, aliases via "A as Alice".
- Annotators: @Actor, @Database, etc.
- Messages can be nested with braces; control flow with if/else, while/for, par, opt, try/catch/finally.
- Comments: // comment.
- Requires external diagram registration (mermaid-zenuml).
