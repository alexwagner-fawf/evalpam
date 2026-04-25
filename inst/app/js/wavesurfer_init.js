console.log('[evalpam] wavesurfer_init loaded — v70 — ' + new Date().toISOString());

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
let _loopSource = null;  // Web Audio filtered loop source; stopped & nulled between loads
let _spectrogramColormap = null; // output palette (blackPoint=0.35) used by normalizeSpectrogramPerRow
let _wsRenderColormap   = null; // linear render palette (blackPoint=0) passed to WaveSurfer colorMap

// ── Magma colour map with noise-floor suppression ────────────────────────
// blackPoint (0–1): amplitudes below this fraction are clipped to black.
//   Equivalent to Raven Pro's "brightness/gain" control. Raising it pushes
//   the noise floor into the dark part of the palette so bird calls pop.
// gamma (<1): power-law compression applied to the range above blackPoint.
//   0.5 = amplitude scale (sqrt), making faint calls above the noise visible.
function createMagmaMap(gamma = 0.5, blackPoint = 0.35, maxBright = 1.0) {
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
    // Subtract noise floor, stretch remaining range, then apply gamma.
    const above      = Math.max(0, (i / 255 - blackPoint) / (1 - blackPoint));
    const compressed = Math.pow(above, gamma);
    // maxBright caps the highest-intensity end of the palette.
    // 1.0 = full inferno (entry 63 → near-white [252,252,252]).
    // 0.78 = entry ~49 → bright orange [229,194,28] — prevents near-white
    // for loud noise (insects, wind) at 4–10 kHz from washing out the palette.
    const maxEntry = maxBright * (inferno64.length - 1);
    const t  = compressed * maxEntry;
    const lo = Math.floor(t);
    const hi = Math.min(lo + 1, inferno64.length - 1);
    const f  = t - lo;
    map.push([
      (inferno64[lo][0] + f * (inferno64[hi][0] - inferno64[lo][0])) / 255,
      (inferno64[lo][1] + f * (inferno64[hi][1] - inferno64[lo][1])) / 255,
      (inferno64[lo][2] + f * (inferno64[hi][2] - inferno64[lo][2])) / 255,
      1
    ]);
  }
  return map;
}

// ── Per-row normalisation ─────────────────────────────────────────────────────
// WaveSurfer uses _wsRenderColormap (blackPoint=0, linear) so bird calls at
// ~2–4% of global max land at canvas R=4 instead of being clipped to R=0 by
// the output colormap's blackPoint=0.45.
//
// Two paths, keyed on range = peak − 5th-pct floor:
//
//  range < FLAT_THRESH  → FORCE-DARK
//    Uniformly silent row (silence, above-frequencyMax fill, or DC artifact).
//    Paint with _spectrogramColormap[0] (near-black).
//
//  range ≥ FLAT_THRESH  → PER-ROW STRETCH
//    Subtract local floor, normalize to [0,1], apply sqrt for perceptual gamma,
//    then map through _spectrogramColormap (blackPoint=0.45 suppresses the lower
//    ~18% of relative amplitude, so the noise floor within each row fades dark
//    while call peaks land at sv=255 → bright orange-white).
//    Works for both faint high-freq calls (range=4) and loud low-freq noise
//    (range=150): any row with temporal variation shows its structure.
//
// specCanvas must be the currently-displayed canvas (found fresh via
// findSpecCanvas() at call time).
let _firstNormDone = false;
function normalizeSpectrogramPerRow(specCanvas) {
  if (!_spectrogramColormap) return 0;
  if (!specCanvas || !specCanvas.parentElement) return 0;
  // Guard: skip if this canvas has already been normalized — prevents the
  // staggered retry timers from re-applying the colormap to already-mapped
  // pixels (which progressively corrupts the palette).
  // Exception: if WaveSurfer has overwritten the canvas with raw bright data
  // (same DOM element, new FFT draw), the reference brightness check below
  // detects this and resets the guard so we normalize the fresh content.
  if (specCanvas._evalpamNormDone) {
    try {
      const ctx0 = specCanvas.getContext('2d');
      const px = ctx0.getImageData(Math.floor(specCanvas.width / 2), 5, 1, 1).data;
      const bright = (px[0] + px[1] + px[2]) / 3;
      // After normalization, center-top is dark (stored in _evalpamNormRef).
      // Raw WaveSurfer render is bright (yellow/orange → avg ~150+).
      // Threshold: 3× ref + 15 catches any genuine redraw while ignoring
      // minor pixel jitter from JPEG/GPU rounding.
      if (bright <= (specCanvas._evalpamNormRef || 0) * 3 + 15) return 0;
      specCanvas._evalpamNormDone = false; // canvas was redrawn with raw data
    } catch(e) { return 0; }
  }

  // A CSS filter on the canvas creates a GPU compositing cache; putImageData
  // writes are invisible until the filter is removed.
  const csf = getComputedStyle(specCanvas).filter;
  if (csf && csf !== 'none') specCanvas.style.filter = 'none';

  const ctx = specCanvas.getContext('2d');
  if (!ctx) return 0;
  const w = specCanvas.width, h = specCanvas.height;
  if (w < 4 || h < 4) return 0;

  let imgData;
  try { imgData = ctx.getImageData(0, 0, w, h); }
  catch(e) { return 0; }
  const data = imgData.data;

  // Pass 1 — per-row floor subtraction.
  // Each frequency band has its own broadband noise pedestal.  Removing the
  // per-row 5th-percentile floor before the global step prevents horizontal
  // stripes: without this, rows with high noise floors get stretched to fill
  // the full colormap range independently, making background noise appear as
  // bright as a bird call in a quieter band.
  //
  // After subtraction the values represent "signal above the local noise
  // floor".  Bird calls — which are narrowband and loud relative to the floor
  // of their frequency band — produce large residuals; broadband background
  // noise (which IS the floor) produces residuals near zero.
  const floorSub = new Uint8Array(w * h); // values 0-255, fits Uint8
  let maxPeak = 0;

  for (let y = 0; y < h; y++) {
    // Compute (R+G+B)/3 per pixel.  Using the RGB average instead of R alone
    // avoids the non-monotonic R channel of the magma colormap at high intensity
    // (entries 47-61: R spans only 227-254 and dips non-monotonically).
    const rowR = new Uint8Array(w);
    for (let x = 0; x < w; x++) {
      const pi = (y * w + x) * 4;
      rowR[x] = (data[pi] + data[pi+1] + data[pi+2]) / 3 | 0;
    }
    // 5th-percentile floor via histogram — O(w) instead of O(w log w) sort.
    const hist = new Uint32Array(256);
    for (let x = 0; x < w; x++) hist[rowR[x]]++;
    const floorTarget = Math.floor(w * 0.05);
    let cumul = 0, floor = 0;
    for (let b = 0; b < 256; b++) { cumul += hist[b]; if (cumul >= floorTarget) { floor = b; break; } }

    const rowPeak = rowR[rowR.reduce((mi, v, i) => v > rowR[mi] ? i : mi, 0)];
    if (rowPeak > maxPeak) maxPeak = rowPeak;

    for (let x = 0; x < w; x++) {
      floorSub[y * w + x] = rowR[x] > floor ? rowR[x] - floor : 0;
    }
  }

  if (maxPeak === 0) {
    console.log('[evalpam] norm skipped — blank canvas');
    return -1;
  }

  // Pass 2 — global peak (99th percentile of all floor-subtracted values).
  // A single global scale preserves relative amplitude across all frequency
  // bands, so the bird call (the loudest residual signal) maps to the bright
  // end of the colormap while broadband noise residuals stay dark.
  // Using the 99th percentile rather than the absolute max makes the scale
  // robust to single-pixel artefacts from clipping or codec rounding; the
  // top 1% is allowed to saturate to maximum brightness.
  const globHist = new Uint32Array(256);
  for (let i = 0; i < floorSub.length; i++) globHist[floorSub[i]]++;
  const p99target = Math.floor(floorSub.length * 0.99);
  let cumul99 = 0, globalPeak = 1;
  for (let b = 0; b < 256; b++) {
    cumul99 += globHist[b];
    if (cumul99 >= p99target) { globalPeak = Math.max(1, b); break; }
  }

  // Pass 3 — map through colormap with single global scale.
  for (let y = 0; y < h; y++) {
    for (let x = 0; x < w; x++) {
      const n  = Math.min(1, floorSub[y * w + x] / globalPeak);
      const sv = Math.round(n * 255);
      const cm = _spectrogramColormap[sv];
      const pi = (y * w + x) * 4;
      data[pi]   = Math.round(cm[0] * 255);
      data[pi+1] = Math.round(cm[1] * 255);
      data[pi+2] = Math.round(cm[2] * 255);
    }
  }

  specCanvas._evalpamNormDone = true;
  const _refIdx = (5 * w + Math.floor(w / 2)) * 4;
  specCanvas._evalpamNormRef = (data[_refIdx] + data[_refIdx+1] + data[_refIdx+2]) / 3;
  ctx.putImageData(imgData, 0, 0);

  console.log('[evalpam] norm done — canvas:', w, 'x', h, 'globalPeak:', globalPeak);
  return h;
}

// Module-scope canvas finder so resize/redraw handlers can call it.
// Among all tall canvases in the WaveSurfer shadow DOM, returns the one with
// the highest R value at the top-eighth row — that's the rendered spectrogram.
// Falls back to the last tall canvas when all are equally blank (post-normalization).
function findSpecCanvas() {
  if (!ws || !ws.getWrapper) return null;
  const wrapper = ws.getWrapper();
  const root = (wrapper && wrapper.shadowRoot) ? wrapper.shadowRoot : wrapper;
  if (!root) return null;
  const all = root.querySelectorAll('canvas');
  let best = null, bestR = -1, fallback = null;
  for (const c of all) {
    if (c.height <= 60) continue;
    fallback = c;
    const ctx = c.getContext('2d');
    if (!ctx) continue;
    try {
      const px = ctx.getImageData(Math.floor(c.width / 2), Math.floor(c.height / 8), 1, 1).data;
      if (px[0] > bestR) { bestR = px[0]; best = c; }
    } catch(_) {}
  }
  return best || fallback;
}

// Debounced re-normalization triggered by window resize.
// Hides the container, waits for WaveSurfer to finish its resize redraw,
// then normalizes and reveals.
let _resizeNormTimer = null;
window.addEventListener('resize', () => {
  clearTimeout(_resizeNormTimer);
  _resizeNormTimer = setTimeout(() => {
    const c = findSpecCanvas();
    if (!c || !c.parentElement) return;
    const container = c.parentElement;
    container.style.opacity = '0';
    const reveal = setTimeout(() => { container.style.opacity = '1'; }, 1000);
    [120, 400].forEach(ms =>
      setTimeout(() => {
        const c2 = findSpecCanvas();
        const changed = normalizeSpectrogramPerRow(c2);
        if (changed > 0 && c2 && c2.parentElement) {
          clearTimeout(reveal);
          c2.parentElement.style.opacity = '1';
        }
      }, ms)
    );
  }, 150); // wait 150 ms after resize stops before acting
});

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
  let _lastLoad = {url: null, freq_max: null, freq_min: null, t: 0};
  Shiny.addCustomMessageHandler('ws_load', function(msg) {
    // Deduplicate: R's observer can fire 2-3× for the same recording when
    // debounced freq inputs settle. Drop any repeat within 1500 ms.
    const now = Date.now();
    if (msg.url === _lastLoad.url &&
        msg.freq_max === _lastLoad.freq_max &&
        msg.freq_min === _lastLoad.freq_min &&
        now - _lastLoad.t < 1500) {
      console.log('[evalpam] ws_load deduped');
      return;
    }
    _lastLoad = {url: msg.url, freq_max: msg.freq_max, freq_min: msg.freq_min, t: now};
    _firstNormDone = false;

    if (_loopSource) { try { _loopSource.stop(); } catch(e) {} _loopSource = null; }
    if (ws) {
      try { ws.setVolume(1); } catch(e) {}
      try { ws.destroy(); } catch(e) {}
      ws = null;
      regionsPlugin = null;
    }

    // Extract numeric spectrogram ID from URLs like "spectrograms/123.mp3"
    // or filtered variants like "spectrograms/123_f0_10000.mp3".
    currentSpecId = msg.url ? (msg.url.match(/\/(\d+)(?:_f\d+_\d+)?\.mp3$/) || [])[1] || null : null;

    regionsPlugin = RegionsPlugin.create();

    // 1024-sample FFT: ~43 Hz/bin at 44.1 kHz, ~23 ms window.
    // Doubles frequency resolution vs 512 so tonal bird calls appear as
    // sharper horizontal bands. The plugin defaults noverlap to fftSamples/2
    // (50% overlap, step = 512 → ~600 columns/7 s); even without overlap
    // (step = 1024, ~300 columns) it stays well clear of the column-stretching
    // artefact seen with fftSamples=2048.
    const fftSamples = 1024;

    const freqMin_eff = Math.max(0, msg.freq_min || 0);
    const freqMax_eff = msg.freq_max || 10000;

    // Decode at the minimum rate whose Nyquist clears frequencyMax (with 10%
    // headroom), capped at the actual source rate.  Decoding below 48 kHz also
    // sharpens frequency resolution: Hz/bin = sampleRate / fftSamples.
    const decodeSampleRate = Math.min(48000, Math.ceil(freqMax_eff * 2.2));

    const spectrogramPlugin = SpectrogramPlugin.create({
      labels: false,
      height: 800,
      fftSamples: fftSamples,
      windowFunc: 'hann',
      frequencyMax: freqMax_eff,
      frequencyMin: freqMin_eff,
      // gainDB / rangeDB define the plugin's dB-to-byte mapping:
      //   floor  = −(gainDB + rangeDB)
      //   clip   = −gainDB
      //   byte   = round((s + gainDB) / rangeDB * 255), clamped via Uint8Array
      //
      // Default gainDB=20 clips at −20 dB.  Outdoor recordings have broadband
      // noise consistently at −20 dB FFT magnitude across 4–10 kHz, so every
      // frame saturates to byte=255 → range=0 → forceDark → painted black.
      //
      // gainDB=0  → clip at 0 dB (unreachable for normalised ≤1.0 audio)
      //            → floor at −100 dB (same as before, no quiet-signal loss)
      //            → noise at −20 dB maps to byte≈205 with real variation
      gainDB: 0,
      rangeDB: 100,
      // sampleRate fallback: prevents the "fill entire canvas with
      // colorMap[255]" path when this.buffer is transiently null.
      sampleRate:  decodeSampleRate,
      colorMap: (
        _spectrogramColormap = createMagmaMap(2, 0.0, 0.8),
        _wsRenderColormap    = createMagmaMap(0.5, 0.0,  1.0)
      )
    });

    ws = WaveSurfer.create({
      container: '#waveform',
      height: 50,
      minPxPerSec: 100,
      scrollParent: true,
      pixelRatio: 1,
      waveColor: '#6db3f2',
      progressColor: '#2980b9',
      cursorColor: '#e74c3c',
      cursorWidth: 2,
      barWidth: 2,
      barGap: 1,
      barRadius: 1,
      normalize: false,
      backend: 'WebAudio',
      sampleRate: decodeSampleRate,
      plugins: [
        spectrogramPlugin,
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

    // Blacken the inner scroll container that ws.getWrapper() returns.
    // NOTE: ws.getWrapper() returns the .wrapper div INSIDE the shadow root,
    // NOT the shadow host — so .shadowRoot on it is always null.
    // We use getRootNode() (available once the element is in a shadow tree)
    // to reach the actual shadow root and inject a CSS rule.
    try {
      const wrapEl = ws.getWrapper();
      if (wrapEl) wrapEl.style.backgroundColor = '#000';
    } catch(_) {}

    // Capture this instance so ready/poll callbacks can bail if a newer
    // ws_load arrived and destroyed this instance.
    const myWs = ws;

    // Loop-playback state for the spectrogram selection box.
    // Set by the mouseup handler; cleared on pause or new ws_load.
    let _loopStart = null, _loopEnd = null;

    // Stop any running frequency-filtered Web Audio loop and restore WaveSurfer volume.
    function _stopFilteredLoop() {
      _loopEnd = null;
      if (_loopSource) { try { _loopSource.stop(); } catch(e) {} _loopSource = null; }
      if (ws) { try { ws.setVolume(1); } catch(e) {} }
    }

    // ── Spectrogram canvas observer ──────────────────────────────────────────
    // MutationObserver fires synchronously before browser repaints, so setting
    // opacity:0 on a newly-added canvas container prevents the user from ever
    // seeing the raw yellow pre-normalization frame — even for canvas1 (the
    // first render) and any subsequent re-renders (canvas2, canvas3, …).
    // The staggered timers in onSpecCanvasReady only handled canvas1 via a
    // requestAnimationFrame (one frame too late); this replaces that mechanism.
    ;(function setupSpecObserver() {
      const wrapEl = ws.getWrapper ? ws.getWrapper() : null;
      if (!wrapEl) return;
      const root = wrapEl.getRootNode ? wrapEl.getRootNode()
                                       : (wrapEl.shadowRoot || wrapEl);

      // Block 'redraw' at the event-bus level.  Patching spectrogramPlugin.render
      // alone is unreliable if the plugin captured the original via .bind(this).
      // Intercepting ws.emit works regardless of subscription style.
      let _blockRedraw = false;
      if (typeof ws.emit === 'function') {
        const _origEmit = ws.emit;
        ws.emit = function(event, ...args) {
          if (_blockRedraw && event === 'redraw') return;
          return _origEmit.call(this, event, ...args);
        };
      }

      const specObs = new MutationObserver(mutations => {
        if (ws !== myWs) { specObs.disconnect(); return; }
        for (const m of mutations) {
          for (const node of m.addedNodes) {
            if (node.nodeType !== 1) continue;
            const candidates = node.tagName === 'CANVAS' ? [node]
              : [...node.querySelectorAll('canvas')];
            for (const tc of candidates) {
              if (tc.height <= 60 || tc._evalpamNormDone) continue;

              // Block ALL future renders immediately when the canvas is first
              // detected — before normalization, before 'ready', before play().
              //
              // Confirmed from SpectrogramPlugin v7.12.6 source:
              //   wavesurfer.on('redraw', () => this.throttledRender())
              // The arrow function does a dynamic this.xxx lookup, so patching
              // spectrogramPlugin.throttledRender intercepts every future call.
              // _blockRedraw guards the ws.emit path as a second barrier.
              //
              // drawSpectrogram() calls clearCanvases() first on every render,
              // physically removing any existing canvas from the DOM.  We must
              // patch before the next render starts, not after normalization.
              _blockRedraw = true;
              try { spectrogramPlugin.throttledRender = () => {}; } catch(e) {}
              try { spectrogramPlugin.render        = () => {}; } catch(e) {}

              // Hide just this canvas (opacity on the element itself, not its
              // parent).  The parent may be spectrogramPlugin.container or a
              // wrapper div that also holds previously-normalized canvases;
              // hiding the parent would blank those good canvases too.
              tc.style.opacity = '0';
              const safeReveal = setTimeout(() => { tc.style.opacity = '1'; }, 3000);
              let normRetries = 0;
              const tryNorm = () => {
                if (ws !== myWs) { clearTimeout(safeReveal); return; }
                if (tc._evalpamNormDone) {
                  clearTimeout(safeReveal);
                  tc.style.opacity = '1';
                  return;
                }
                const changed = normalizeSpectrogramPerRow(tc);
                if (changed > 0) {
                  clearTimeout(safeReveal);
                  tc.style.opacity = '1';
                  // Remove stale raw WaveSurfer canvases at tc's own level.
                  // Preserve our sel-canvas overlay and any other non-canvas
                  // elements (e.g. spec-overlay div) that onSpecCanvasReady
                  // appended to the same parent after 'ready' fired (~16 ms
                  // before this first tryNorm at 80 ms).
                  try {
                    if (tc.parentElement) {
                      for (const sib of [...tc.parentElement.children]) {
                        if (sib !== tc &&
                            sib.tagName === 'CANVAS' &&
                            !sib.classList.contains('sel-canvas')) {
                          sib.remove();
                        }
                      }
                    }
                  } catch(e) {}
                  // Also remove stale wrapper divs one level up — but only
                  // if tc is not a direct child of spectrogramPlugin.container,
                  // to avoid accidentally removing tc's own parent div.
                  try {
                    const pp = tc.parentElement && tc.parentElement.parentElement;
                    if (pp && tc.parentElement !== spectrogramPlugin.container) {
                      for (const sib of [...pp.children]) {
                        if (sib !== tc.parentElement) {
                          const sc = sib.querySelector('canvas');
                          if (sc && sc.height > 60) sib.remove();
                        }
                      }
                    }
                  } catch(e) {}
                } else if (changed < 0 && normRetries++ < 15) {
                  setTimeout(tryNorm, 250);
                } else if (normRetries >= 15) {
                  tc.style.opacity = '1';
                }
              };
              [80, 300, 700, 1500].forEach(d => setTimeout(tryNorm, d));
            }
          }
        }
      });
      specObs.observe(root, { childList: true, subtree: true });
    })();

    ws.on('ready', function() {
      // Guard against stale ready events from a destroyed instance.
      if (ws !== myWs) return;

      const duration = ws.getDuration();
      console.log('[evalpam] ws ready — duration:', duration,
        'freq_min:', freqMin_eff, 'freq_max:', freqMax_eff,
        'detection_start:', msg.detection_start);

      updateControls();

      // ── Canvas-dependent work (detection lines + selection overlay) ──────
      // The SpectrogramPlugin creates canvas elements asynchronously inside
      // drawSpectrogram(), which runs after ready. Poll with rAF until a
      // canvas with non-zero layout width exists in #spectrogram.
      // PLUGIN_HEIGHT must match the height: option passed to SpectrogramPlugin.create().
      const PLUGIN_HEIGHT = 800;
      const specIdForSel = currentSpecId;
      let selAttempts = 0;

      function onSpecCanvasReady(specCanvas) {
        const duration2 = ws ? ws.getDuration() : duration;

        // The SpectrogramPlugin sizes the canvas intrinsic height as
        //   options.height × (freqMax - freqMin) / Nyquist
        // which is smaller than PLUGIN_HEIGHT whenever freqMax < Nyquist,
        // leaving a blank gap at the top of the wrapper.
        // Fix: force CSS height to the exact pixel value — absolute px
        // always resolves correctly inside a Shadow DOM, unlike '100%'
        // which requires the parent to have a definite CSS height.
        specCanvas.style.width  = '100%';
        specCanvas.style.height = PLUGIN_HEIGHT + 'px';

        // Blacken only the canvas's own ancestor chain:
        //   canvas → canvasContainer → plugin wrapper → .wrapper → .scroll
        // The shadow DOM also contains .progress and .cursor (position:absolute,
        // z-index 2/5) which must stay transparent — a broad CSS injection that
        // targets all divs covers those and makes everything opaque black.
        // Targeting only the 4 direct ancestors of the canvas is safe.
        const _ancestors = [
          specCanvas,
          specCanvas.parentElement,                   // canvasContainer
          specCanvas.parentElement?.parentElement,    // plugin wrapper
          specCanvas.parentElement?.parentElement?.parentElement,  // .wrapper
          specCanvas.parentElement?.parentElement?.parentElement?.parentElement  // .scroll
        ];
        for (const el of _ancestors) {
          if (el) el.style.backgroundColor = '#000';
        }

        const cw = Math.round(specCanvas.getBoundingClientRect().width)
                   || specCanvas.offsetWidth || specCanvas.width;
        // Use the known plugin height rather than getBoundingClientRect().
        // The plugin may not have applied the forced CSS height yet (layout
        // can be async), so hardcoding avoids a stale/zero value.
        const ch = PLUGIN_HEIGHT;

        console.log('[evalpam] specCanvas found —',
          'intrinsic:', specCanvas.width, 'x', specCanvas.height,
          'display:', cw, 'x', ch,
          'parent:', specCanvas.parentElement?.tagName,
          'grandparent:', specCanvas.parentElement?.parentElement?.id);

        // Normalization and opacity-hide are now handled by the MutationObserver
        // (setupSpecObserver) set up before ws.on('ready').  The observer fires
        // synchronously before browser repaints, hiding each canvas before the
        // user can see the raw yellow render, and normalizing once FFT data lands.

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
        sel.height = PLUGIN_HEIGHT;  // intrinsic = CSS height → 1:1 pixel mapping
        sel.style.cssText =
          'position:absolute;top:0;left:0;width:100%;height:' + PLUGIN_HEIGHT + 'px;cursor:crosshair;z-index:20;';
        specParent.appendChild(sel);

        const ctx2    = sel.getContext('2d');
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

          // The SpectrogramPlugin renders with scale='mel' (the default).
          // The canvas y-axis is LINEAR in mel space:
          //   y=0           → mel(freqMax_eff)
          //   y=sel.height  → mel(freqMin_eff)
          // To get the correct Hz we invert: freq = mel⁻¹(mel interpolated at y).
          const hzToMel = f => 2595 * Math.log10(1 + Math.max(0, f) / 700);
          const melToHz = m => 700 * (Math.pow(10, Math.max(0, m) / 2595) - 1);
          const melMax  = hzToMel(freqMax_eff);
          const melMin  = hzToMel(freqMin_eff);
          const fMax2   = melToHz(melMax - (ry        / sel.height) * (melMax - melMin));
          const fMin2   = melToHz(melMax - ((ry + rh) / sel.height) * (melMax - melMin));

          console.log('[evalpam] selection coords —',
            'ry:', ry.toFixed(1), 'rh:', rh.toFixed(1),
            'sel.height:', sel.height,
            'freqMax_eff:', freqMax_eff, 'freqMin_eff:', freqMin_eff,
            '→ fMax2:', Math.round(fMax2), 'fMin2:', Math.round(fMin2));

          const payload = {
            spec_id: specIdForSel,
            t_start: +tStart.toFixed(3),
            t_end:   +tEnd.toFixed(3),
            f_min:   Math.round(Math.max(0, fMin2)),
            f_max:   Math.round(fMax2)
          };
          console.log('[evalpam] spec_selection →', JSON.stringify(payload));
          Shiny.setInputValue('spec_selection', payload, { priority: 'event' });

          // Frequency-filtered loop via Web Audio API.
          // AudioBufferSourceNode (loop=true, loopStart/loopEnd) → highpass →
          // lowpass → destination.  The decoded buffer is already in memory so
          // there is no server round-trip and no latency.
          // WaveSurfer is muted so its audio does not double the filtered source;
          // ws.play() is still called so the cursor stays in sync, and the
          // audioprocess handler seeks the cursor back at loopEnd.
          if (ws) {
            _stopFilteredLoop();
            _loopStart = tStart;
            _loopEnd   = tEnd;

            const audioBuffer = ws.getDecodedData ? ws.getDecodedData() : null;
            if (audioBuffer && selectionAudioCtx) {
              const src      = selectionAudioCtx.createBufferSource();
              src.buffer     = audioBuffer;
              src.loop       = true;
              src.loopStart  = tStart;
              src.loopEnd    = tEnd;

              const hp = selectionAudioCtx.createBiquadFilter();
              hp.type = 'highpass';
              hp.frequency.value = Math.max(20, Math.round(Math.max(0, fMin2)));

              const lp = selectionAudioCtx.createBiquadFilter();
              lp.type = 'lowpass';
              lp.frequency.value = Math.min(
                selectionAudioCtx.sampleRate * 0.499,
                Math.round(fMax2)
              );

              src.connect(hp);
              hp.connect(lp);
              lp.connect(selectionAudioCtx.destination);
              src.start(0, tStart);
              _loopSource = src;

              try { ws.setVolume(0); } catch(e) {}
              console.log('[evalpam] filtered loop —',
                tStart.toFixed(2), '–', tEnd.toFixed(2), 's |',
                Math.round(Math.max(0, fMin2)), '–', Math.round(fMax2), 'Hz');
            }
            ws.play(tStart);
          }
        });
      } // end onSpecCanvasReady

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
        // Any 'redraw' this triggers is silenced by the spectrogramPlugin.render
        // no-op patch applied in setupSpecObserver after first normalization.
      });
    });

    ws.on('audioprocess', updateControls);
    ws.on('audioprocess', ct => {
      if (_loopEnd !== null && ct >= _loopEnd) ws.setTime(_loopStart);
    });
    ws.on('seeking',      updateControls);
    ws.on('play',         updateControls);
    ws.on('pause',        () => { updateControls(); _stopFilteredLoop(); });

    // 'redraw' handler intentionally removed (v61).
    // It set opacity:0 on already-normalized canvases whenever ws.play()
    // triggered a waveform redraw, then raced against the spectrogram plugin's
    // in-progress re-render — causing the "jump to yellow, never recovers" bug.
    // The MutationObserver (setupSpecObserver) handles new canvases from any
    // re-render.  The brightness check inside normalizeSpectrogramPerRow handles
    // same-canvas redraws.  The window resize listener handles layout changes.
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
