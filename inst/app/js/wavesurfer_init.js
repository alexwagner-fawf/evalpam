console.log('[evalpam] wavesurfer_init loaded — v8 — ' + new Date().toISOString());

// Dynamic imports so this file can be loaded as a plain <script> by
// bundle_resources() without triggering "import only at top level of module".
// Imports start loading immediately; the shiny:connected handler awaits them.
const _wsReady = Promise.all([
  import('https://unpkg.com/wavesurfer.js@7/dist/wavesurfer.esm.js'),
  import('https://unpkg.com/wavesurfer.js@7/dist/plugins/spectrogram.esm.js'),
  import('https://unpkg.com/wavesurfer.js@7/dist/plugins/regions.esm.js'),
  import('https://unpkg.com/wavesurfer.js@7/dist/plugins/timeline.esm.js')
]);

let ws = null;
let regionsPlugin = null;
let currentSpecId = null;
let selectionAudioCtx = null;

// Inferno Palette (matplotlib Standard – bewährt für Spektrogramme)
function createMagmaMap() {
  const inferno64 = [
    [0,0,4],[2,1,10],[5,2,19],[10,3,28],[16,4,38],[23,4,48],[30,5,56],[38,5,63],
    [46,5,68],[54,5,72],[62,4,75],[70,4,77],[78,4,78],[85,5,79],[93,6,79],[100,9,78],
    [107,12,77],[114,16,76],[121,20,74],[128,25,71],[134,30,69],[140,36,66],[146,41,62],
    [152,47,59],[157,53,55],[163,59,52],[168,65,48],[173,72,44],[178,78,40],[183,85,37],
    [188,92,33],[192,99,29],[196,106,26],[201,114,22],[205,121,19],[209,129,16],[212,137,14],
    [216,145,12],[219,153,11],[222,161,11],[224,169,13],[226,177,16],[228,186,21],[229,194,28],
    [230,202,37],[230,210,47],[230,218,59],[229,226,72],[228,233,87],[227,240,103],
    [227,246,120],[229,251,137],[233,254,154],[239,255,170],[245,255,185],[250,254,199],
    [253,253,211],[254,251,223],[254,249,233],[252,248,242],[252,248,248],[252,252,252]
  ];
  const map = [];
  for (let i = 0; i < 256; i++) {
    const t = i / 255 * (inferno64.length - 1);
    const lo = Math.floor(t);
    const hi = Math.min(lo + 1, inferno64.length - 1);
    const f = t - lo;
    map.push([
      (inferno64[lo][0] + f * (inferno64[hi][0] - inferno64[lo][0])) / 255,
      (inferno64[lo][1] + f * (inferno64[hi][1] - inferno64[lo][1])) / 255,
      (inferno64[lo][2] + f * (inferno64[hi][2] - inferno64[lo][2])) / 255,
      1
    ]);
  }
  return map;
}

function fmtTime(sec) {
  if (sec == null || isNaN(sec)) return '0:00.0';
  const m = Math.floor(sec / 60);
  const s = (sec % 60).toFixed(1).padStart(4, '0');
  return m + ':' + s;
}

function updateControls() {
  if (!ws) return;
  const cur = document.getElementById('ws-current-time');
  const dur = document.getElementById('ws-total-time');
  const btn = document.getElementById('ws-play-btn');
  if (cur) cur.textContent = fmtTime(ws.getCurrentTime());
  if (dur) dur.textContent = fmtTime(ws.getDuration());
  if (btn) btn.textContent = ws.isPlaying() ? '⏸' : '▶';
}

$(document).on('shiny:connected', async function() {

  // Resolve wavesurfer modules (already loading since page start)
  const [
    { default: WaveSurfer },
    { default: SpectrogramPlugin },
    { default: RegionsPlugin },
    { default: TimelinePlugin }
  ] = await _wsReady;

  // Play Button
  $(document).on('click', '#ws-play-btn', function() {
    if (ws) ws.playPause();
  });

  // ---- Laden ----
  Shiny.addCustomMessageHandler('ws_load', function(msg) {
    if (ws) {
      try { ws.destroy(); } catch(e) {}
      ws = null;
      regionsPlugin = null;
    }

    // Extract numeric spectrogram ID from URLs like "spectrograms/123.mp3"
    // or filtered variants like "spectrograms/123_f0_10000.mp3".
    currentSpecId = msg.url ? (msg.url.match(/\/(\d+)(?:_f\d+_\d+)?\.mp3$/) || [])[1] || null : null;

    regionsPlugin = RegionsPlugin.create();

    // The browser's WebAudio always decodes to ~44100 Hz regardless of the
    // source file's sample rate. fftSamples must be sized for 44100 Hz.
    // 2048 → ~46 ms window, ~21.5 Hz/bin — good for bird-call detail.
    const fftSamples = 2048;

    ws = WaveSurfer.create({
      container: '#waveform',
      height: 50,
      minPxPerSec: 1,       // render at container width, not audio-length-driven default
      waveColor: '#6db3f2',
      progressColor: '#2980b9',
      cursorColor: '#e74c3c',
      cursorWidth: 2,
      barWidth: 2,
      barGap: 1,
      barRadius: 1,
      normalize: true,
      backend: 'WebAudio',
      plugins: [
        SpectrogramPlugin.create({
          labels: false,
          height: 500,
          fftSamples: fftSamples,
          windowFunc: 'hann',
          // Cap at actual Nyquist of the recording (server sends sample_rate).
          // This prevents showing empty frequency bins above the source's Nyquist.
          frequencyMax: Math.min(msg.freq_max || 10000,
                                 (msg.sample_rate || 44100) / 2),
          frequencyMin: msg.freq_min || 0,
          colorMap: createMagmaMap()
        }),
        TimelinePlugin.create({
          container: '#timeline',
          primaryLabelInterval: 1,
          secondaryLabelInterval: 0.5,
          primaryColor: '#aaa',
          secondaryColor: '#666',
          primaryFontColor: '#aaa',
          secondaryFontColor: '#666',
          timeInterval: 0.5,
          style: { fontSize: '11px', color: '#aaa' }
        }),
        regionsPlugin
      ]
    });

    // Capture this instance so ready/poll callbacks can bail if a newer
    // ws_load arrived and destroyed this instance.
    const myWs = ws;

    ws.on('ready', function() {
      // Guard against stale ready events from a destroyed instance.
      if (ws !== myWs) return;

      const duration = ws.getDuration();
      console.log('[evalpam] ws ready — duration:', duration,
        'freq_max:', msg.freq_max,
        'detection_start:', msg.detection_start);

      updateControls();

      // ── Canvas-dependent work (detection lines + selection overlay) ──────
      // The SpectrogramPlugin creates canvas elements asynchronously inside
      // drawSpectrogram(), which runs after ready. Poll with rAF until a
      // canvas with non-zero layout width exists in #spectrogram.
      const SPEC_HEIGHT = 500;
      const specIdForSel = currentSpecId;
      let selAttempts = 0;

      function onSpecCanvasReady(specCanvas) {
        const duration2 = ws ? ws.getDuration() : duration;
        const cw = Math.round(specCanvas.getBoundingClientRect().width)
                   || specCanvas.offsetWidth || specCanvas.width;
        const ch = Math.round(specCanvas.getBoundingClientRect().height)
                   || specCanvas.offsetHeight || SPEC_HEIGHT;

        console.log('[evalpam] specCanvas found —',
          'intrinsic:', specCanvas.width, 'x', specCanvas.height,
          'display:', cw, 'x', ch,
          'parent:', specCanvas.parentElement?.tagName,
          'grandparent:', specCanvas.parentElement?.parentElement?.id);

        // ── Detection marker lines ────────────────────────────────────────
        if (msg.detection_start != null) {
          const pxPerSec = cw / duration2;
          const dStart   = msg.detection_start * pxPerSec;
          const dEnd     = msg.detection_end   * pxPerSec;

          const overlay  = document.createElement('div');
          overlay.id = 'spec-overlay';
          overlay.style.cssText =
            'position:absolute;top:0;left:0;right:0;bottom:0;pointer-events:none;z-index:10;';
          const lineL = document.createElement('div');
          lineL.style.cssText =
            `position:absolute;top:0;left:${dStart}px;width:1px;height:100%;background:rgba(46,204,113,0.7);`;
          const lineR = document.createElement('div');
          lineR.style.cssText =
            `position:absolute;top:0;left:${dEnd}px;width:1px;height:100%;background:rgba(46,204,113,0.7);`;

          const specParent = specCanvas.parentElement;
          specParent.style.position = 'relative';
          const oldOverlay = specParent.querySelector('#spec-overlay');
          if (oldOverlay) oldOverlay.remove();
          overlay.append(lineL, lineR);
          specParent.appendChild(overlay);
        }

        // ── Frequency × time selection canvas ────────────────────────────
        if (!specIdForSel) return;

        const specParent = specCanvas.parentElement;
        specParent.style.position = 'relative';

        const old = specParent.querySelector('.sel-canvas');
        if (old) old.remove();

        const sel = document.createElement('canvas');
        sel.className = 'sel-canvas';
        sel.width  = cw;
        sel.height = ch;
        sel.style.cssText =
          'position:absolute;top:0;left:0;width:100%;height:100%;cursor:crosshair;z-index:20;';
        specParent.appendChild(sel);

        const ctx2    = sel.getContext('2d');
        const freqMax = msg.freq_max || 10000;
        let dragging  = false, sx = 0, sy = 0;

        sel.addEventListener('mousedown', e => {
          e.stopPropagation();
          const r = sel.getBoundingClientRect();
          sx = e.clientX - r.left;
          sy = e.clientY - r.top;
          dragging = true;
          ctx2.clearRect(0, 0, sel.width, sel.height);
        });

        sel.addEventListener('mousemove', e => {
          if (!dragging) return;
          e.stopPropagation();
          const r  = sel.getBoundingClientRect();
          const ex = e.clientX - r.left;
          const ey = e.clientY - r.top;
          ctx2.clearRect(0, 0, sel.width, sel.height);
          ctx2.strokeStyle = 'rgba(46,204,113,0.9)';
          ctx2.lineWidth   = 1.5;
          ctx2.setLineDash([4, 2]);
          ctx2.fillStyle   = 'rgba(46,204,113,0.08)';
          const rx = Math.min(sx, ex), ry = Math.min(sy, ey);
          const rw = Math.abs(ex - sx), rh = Math.abs(ey - sy);
          ctx2.fillRect(rx, ry, rw, rh);
          ctx2.strokeRect(rx, ry, rw, rh);
        });

        sel.addEventListener('mouseup', e => {
          if (!dragging) return;
          e.stopPropagation();
          dragging = false;
          const r  = sel.getBoundingClientRect();
          const ex = e.clientX - r.left;
          const ey = e.clientY - r.top;
          const rx = Math.min(sx, ex), ry = Math.min(sy, ey);
          const rw = Math.abs(ex - sx), rh = Math.abs(ey - sy);
          if (rw < 5 || rh < 5) { ctx2.clearRect(0, 0, sel.width, sel.height); return; }

          // Pre-create / resume AudioContext NOW, inside the user gesture,
          // so it is ready before the server round-trip completes.
          if (!selectionAudioCtx || selectionAudioCtx.state === 'closed') {
            selectionAudioCtx = new (window.AudioContext || window.webkitAudioContext)();
          } else if (selectionAudioCtx.state === 'suspended') {
            selectionAudioCtx.resume();
          }

          const dur    = ws ? ws.getDuration() : duration2;
          const tStart = (rx / sel.width) * dur;
          const tEnd   = ((rx + rw) / sel.width) * dur;
          const fMax2  = freqMax * (1 - ry / sel.height);
          const fMin2  = freqMax * (1 - (ry + rh) / sel.height);

          const payload = {
            spec_id: specIdForSel,
            t_start: +tStart.toFixed(3),
            t_end:   +tEnd.toFixed(3),
            f_min:   Math.round(Math.max(0, fMin2)),
            f_max:   Math.round(fMax2)
          };
          console.log('[evalpam] spec_selection →', JSON.stringify(payload));
          Shiny.setInputValue('spec_selection', payload, { priority: 'event' });
        });
      } // end onSpecCanvasReady

      // wavesurfer v7 attaches a Shadow Root to its wrapper element.
      // Regular querySelectorAll('canvas') finds nothing — we must query
      // inside the shadow root. The spectrogram canvas has intrinsic
      // height 500 while waveform canvases are ~50px.
      function findSpecCanvas() {
        if (!ws || !ws.getWrapper) return null;
        const wrapper = ws.getWrapper();
        // getWrapper() may return the shadow host or the shadow root itself
        const root = (wrapper && wrapper.shadowRoot) ? wrapper.shadowRoot : wrapper;
        if (!root) return null;
        const all = root.querySelectorAll('canvas');
        for (const c of all) {
          if (c.height >= 400) return c;
        }
        return null;
      }

      function pollForSpecCanvas() {
        if (ws !== myWs) return; // stale
        const specCanvas = findSpecCanvas();
        if (!specCanvas) {
          if (++selAttempts < 200) requestAnimationFrame(pollForSpecCanvas);
          else console.warn('[evalpam] gave up waiting for spectrogram canvas after 200 frames');
          return;
        }
        const cw = Math.round(specCanvas.getBoundingClientRect().width)
                   || specCanvas.offsetWidth || specCanvas.width;
        if (cw === 0) {
          if (++selAttempts < 200) requestAnimationFrame(pollForSpecCanvas);
          return;
        }
        onSpecCanvasReady(specCanvas);
      }
      requestAnimationFrame(pollForSpecCanvas);
      // ─────────────────────────────────────────────────────────────────────

      if (msg.seek != null && msg.seek > 0) ws.setTime(msg.seek);
      ws.play().catch(() => {
        // Autoplay may be blocked by the browser until user interacts with the page.
        // The spectrogram still renders; user can click ▶ to start playback.
      });
    });

    ws.on('audioprocess', updateControls);
    ws.on('seeking',      updateControls);
    ws.on('play',         updateControls);
    ws.on('pause',        updateControls);
    // NOTE: the 'interaction' playPause handler is intentionally removed.
    // It fired for every mouseup inside the shadow root — including selection
    // canvas drags — causing spurious seek + play/pause toggles that
    // interrupted selection playback. Use the ▶ button instead.

    ws.load(msg.url);
  });

  Shiny.addCustomMessageHandler('ws_play',  function(_msg) { if (ws) ws.play(); });
  Shiny.addCustomMessageHandler('ws_pause', function(_msg) { if (ws) ws.pause(); });
  Shiny.addCustomMessageHandler('ws_seek',  function(msg) { if (ws) ws.setTime(msg.time); });

  // Plays the server-filtered selection as a base64 MP3 data-URL.
  Shiny.addCustomMessageHandler('ws_play_selection', function(msg) {
    if (!msg.dataUrl) return;
    // Reuse the AudioContext that was pre-created in the mouseup handler
    // (inside the user gesture). Creating a new one here would be blocked
    // by the browser's autoplay policy since we're now outside a gesture.
    if (!selectionAudioCtx) {
      selectionAudioCtx = new (window.AudioContext || window.webkitAudioContext)();
    }
    const ctx = selectionAudioCtx;
    fetch(msg.dataUrl)
      .then(r => r.arrayBuffer())
      .then(buf => ctx.decodeAudioData(buf))
      .then(decoded => {
        const src = ctx.createBufferSource();
        src.buffer = decoded;
        src.connect(ctx.destination);
        src.start(0);
        console.log('[evalpam] playing selection audio, duration:', decoded.duration.toFixed(2));
      })
      .catch(e => console.warn('Selection playback error:', e));
  });
});
