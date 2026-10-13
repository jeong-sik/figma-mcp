const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const os = require('node:os');
const { spawnSync } = require('node:child_process');
const test = require('node:test');

const helper = path.join(__dirname, '../../studio/preview.py');
function fixture() {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'studio-preview-'));
  const input = path.join(dir, 'result.json');
  const created = spawnSync('python3', ['-c', `
import base64,hashlib,json,struct,sys,zlib
chunk=lambda name,data: struct.pack('>I',len(data))+name+data+struct.pack('>I',zlib.crc32(name+data)&0xffffffff)
png=b'\\x89PNG\\r\\n\\x1a\\n'+chunk(b'IHDR',struct.pack('>2I5B',16,16,8,6,0,0,0))+chunk(b'IDAT',zlib.compress((b'\\x00'+bytes([70,110,230,255])*16)*16))+chunk(b'IEND',b'')
json.dump({'content':[{'type':'image','mimeType':'image/png','data':base64.b64encode(png).decode()}], 'structuredContent':{'preview':{'sha256':hashlib.sha256(png).hexdigest()}}}, open(sys.argv[1],'w'))
`, input]);
  assert.equal(created.status, 0, created.stderr.toString());
  return { dir, input, clean: () => fs.rmSync(dir, { recursive: true }) };
}

test('preview exports exactly the image authenticated by the snapshot hash', () => {
  const f = fixture();
  try {
    const output = path.join(f.dir, 'preview.png');
    const result = spawnSync('python3', [helper, f.input, '--output', output]);
    assert.equal(result.status, 0, result.stderr.toString());
    const source = JSON.parse(fs.readFileSync(f.input));
    assert.deepEqual(fs.readFileSync(output), Buffer.from(source.content[0].data, 'base64'));
    source.structuredContent.preview.sha256 = '0'.repeat(64);
    fs.writeFileSync(f.input, JSON.stringify(source));
    const rejected = spawnSync('python3', [helper, f.input, '--output', path.join(f.dir, 'bad.png')]);
    assert.equal(rejected.status, 1);
    assert.equal(fs.existsSync(path.join(f.dir, 'bad.png')), false);
  } finally { f.clean(); }
});

test('installed timg emits Kitty graphics for the retained preview', t => {
  if (spawnSync('timg', ['--version']).error) return t.skip('timg is not installed');
  const f = fixture();
  try {
    const result = spawnSync('python3', [helper, f.input, '--kitty', '--size', '80', '24']);
    assert.equal(result.status, 0, result.stderr.toString());
    assert.ok(result.stdout.includes(Buffer.from('\x1b_G')), 'actual timg output contains Kitty APC');
  } finally { f.clean(); }
});
