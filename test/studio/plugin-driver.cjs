// JSON-lines adapter for the real Plugin entrypoint and controlled native fixture.
const readline = require('node:readline');
const { studio } = require('./fixture.cjs');
const s = studio();
console.log(JSON.stringify({ session_id: s.session, root_id: s.frame.id, document: {
  name: s.figma.root.name, page_id: s.page.id, selection: [s.frame.id],
  editor_type: 'figma', mode: 'default', can_edit: true
} }));
let queue = Promise.resolve();
readline.createInterface({ input: process.stdin }).on('line', line => {
  queue = queue.then(async () => {
    const command = JSON.parse(line);
    console.log(JSON.stringify(await s.send(command.kind, command, command.id)));
  }).catch(error => { console.error(error); process.exitCode = 1; });
});
