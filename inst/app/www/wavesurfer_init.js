import WaveSurfer from 'https://unpkg.com/wavesurfer.js@7/dist/wavesurfer.esm.js';
import SpectrogramPlugin from 'https://unpkg.com/wavesurfer.js@7/dist/plugins/spectrogram.esm.js';
import RegionsPlugin from 'https://unpkg.com/wavesurfer.js@7/dist/plugins/regions.esm.js';
import TimelinePlugin from 'https://unpkg.com/wavesurfer.js@7/dist/plugins/timeline.esm.js';

let ws = null;
let regionsPlugin = null;

// Inferno Palette (matplotlib Standard – bewährt für Spektrogramme)
function createMagmaMap() {
  // 64 Stützpunkte aus matplotlib inferno, interpoliert auf 256
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

// Zeit formatieren (m:ss.s)
function fmtTime(sec) {
  if (sec == null || isNaN(sec)) return '0:00.0';
  const m = Math.floor(sec / 60);
  const s = (sec % 60).toFixed(1).padStart(4, '0');
  return m + ':' + s;
}

// UI-Elemente updaten
function updateControls() {
  if (!ws) return;
  const cur = document.getElementById('ws-current-time');
  const dur = document.getElementById('ws-total-time');
  const btn = document.getElementById('ws-play-btn');
  if (cur) cur.textContent = fmtTime(ws.getCurrentTime());
  if (dur) dur.textContent = fmtTime(ws.getDuration());
  if (btn) btn.textContent = ws.isPlaying() ? '⏸' : '▶';
}

$(document).on('shiny:connected', function() {

  // Play Button
  $(document).on('click', '#ws-play-btn', function() {
    if (ws) ws.playPause();
  });

  // ---- Laden ----
  Shiny.addCustomMessageHandler('ws_load', function(msg) {
    // Alte Instanz sauber zerstören
    if (ws) {
      try { ws.destroy(); } catch(e) {}
      ws = null;
      regionsPlugin = null;
    }

    // Spectrogram Container leeren (verhindert Artefakte)
    const specContainer = document.getElementById('spectrogram');
    if (specContainer) specContainer.innerHTML = '';

    regionsPlugin = RegionsPlugin.create();

    ws = WaveSurfer.create({
      container: '#waveform',
      height: 50,
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
          container: '#spectrogram',
          labels: true,
          labelsColor: '#ddd',
          labelsBackground: 'rgba(0,0,0,0.6)',
          height: 500,
          fftSamples: 2048,
          windowFunc: 'hann',
          frequencyMax: msg.freq_max || 15000,
          frequencyMin: 0,
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
          fontSize: 11,
          timeInterval: 0.5,
          style: {
            fontSize: '11px',
            color: '#aaa'
          }
        }),
        regionsPlugin
      ]
    });

    // Events
    ws.on('ready', function() {
      const duration = ws.getDuration();

      // Padding-Bereiche markieren (NUR auf Waveform, nicht Spectrogram)
      if (msg.detection_start != null && msg.detection_end != null) {
        // Links: Padding
        if (msg.detection_start > 0) {
          regionsPlugin.addRegion({
            start: 0,
            end: msg.detection_start,
            color: 'rgba(100, 100, 100, 0.15)',
            drag: false, resize: false
          });
        }
        // Rechts: Padding
        if (msg.detection_end < duration) {
          regionsPlugin.addRegion({
            start: msg.detection_end,
            end: duration,
            color: 'rgba(100, 100, 100, 0.15)',
            drag: false, resize: false
          });
        }
        // Detektionsfenster (keine Füllung auf Waveform)

      }

      // Vertikale Markierungen auf dem Spektrogramm
      const specEl = document.querySelector('#spectrogram canvas');
      if (specEl && msg.detection_start != null) {
        const overlay = document.createElement('div');
        overlay.id = 'spec-overlay';
        overlay.style.cssText = 'position:absolute; top:0; left:0; right:0; bottom:0; pointer-events:none;';

        const pxPerSec = specEl.clientWidth / duration;
        const dStart = msg.detection_start * pxPerSec;
        const dEnd = msg.detection_end * pxPerSec;

        // Dünne Markierungslinien am Detektionsfenster
        const lineL = document.createElement('div');
        lineL.style.cssText = `position:absolute; top:0; left:${dStart}px; width:1px; height:100%; background:rgba(46,204,113,0.7);`;
        const lineR = document.createElement('div');
        lineR.style.cssText = `position:absolute; top:0; left:${dEnd}px; width:1px; height:100%; background:rgba(46,204,113,0.7);`;

        const specParent = specEl.parentElement;
        specParent.style.position = 'relative';
        overlay.append(lineL, lineR);
        specParent.appendChild(overlay);
      }

      updateControls();

      // Seek & Play
      if (msg.seek != null && msg.seek > 0) {
        ws.setTime(msg.seek);
      }
      ws.play();
    });

    ws.on('audioprocess', updateControls);
    ws.on('seeking', updateControls);
    ws.on('play', updateControls);
    ws.on('pause', updateControls);

    ws.on('interaction', function() {
      ws.playPause();
    });

    ws.load(msg.url);
  });

  // ---- Steuerung ----
  Shiny.addCustomMessageHandler('ws_play', function(msg) {
    if (ws) ws.play();
  });
  Shiny.addCustomMessageHandler('ws_pause', function(msg) {
    if (ws) ws.pause();
  });
  Shiny.addCustomMessageHandler('ws_seek', function(msg) {
    if (ws) ws.setTime(msg.time);
  });
});
