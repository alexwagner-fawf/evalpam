import WaveSurfer from 'https://unpkg.com/wavesurfer.js@7/dist/wavesurfer.esm.js';
import SpectrogramPlugin from 'https://unpkg.com/wavesurfer.js@7/dist/plugins/spectrogram.esm.js';
import RegionsPlugin from 'https://unpkg.com/wavesurfer.js@7/dist/plugins/regions.esm.js';

let ws = null;
let regionsPlugin = null;

// Farbpalette für Spektrogramm (dunkel → hell, vogelstimmen-optimiert)
function createColorMap() {
  const map = [];
  for (let i = 0; i < 256; i++) {
    const t = i / 255;
    if (t < 0.33) {
      // Schwarz → Dunkelblau
      const s = t / 0.33;
      map.push([0, 0, s * 0.4, 1]);
    } else if (t < 0.66) {
      // Dunkelblau → Gelb
      const s = (t - 0.33) / 0.33;
      map.push([s, s * 0.8, 0.4 * (1 - s), 1]);
    } else {
      // Gelb → Weiß
      const s = (t - 0.66) / 0.34;
      map.push([1, 0.8 + s * 0.2, s, 1]);
    }
  }
  return map;
}

$(document).on('shiny:connected', function() {

  // ---- Laden & Abspielen ----
  Shiny.addCustomMessageHandler('ws_load', function(msg) {
    // Alte Instanz sauber zerstören (Memory Leak verhindern)
    if (ws) {
      try { ws.destroy(); } catch(e) {}
      ws = null;
      regionsPlugin = null;
    }

    regionsPlugin = RegionsPlugin.create();

    ws = WaveSurfer.create({
      container: '#waveform',
      height: 60,
      waveColor: '#337ab7',
      progressColor: '#1a5276',
      cursorColor: '#d9534f',
      cursorWidth: 2,
      barWidth: 1,
      normalize: true,
      plugins: [
        SpectrogramPlugin.create({
          container: '#spectrogram',
          labels: true,
          labelsColor: '#ccc',
          labelsBackground: 'rgba(0,0,0,0.7)',
          height: 400,
          fftSamples: 1024,
          frequencyMax: msg.freq_max || 15000,
          colorMap: createColorMap()
        }),
        regionsPlugin
      ]
    });

    // Events
    ws.on('ready', function() {
      // Detektionsfenster markieren
      if (msg.detection_start != null && msg.detection_end != null) {
        // Padding-Bereiche abdunkeln (links)
        if (msg.detection_start > 0) {
          regionsPlugin.addRegion({
            start: 0,
            end: msg.detection_start,
            color: 'rgba(0, 0, 0, 0.35)',
            drag: false,
            resize: false
          });
        }
        // Padding-Bereiche abdunkeln (rechts)
        const duration = ws.getDuration();
        if (msg.detection_end < duration) {
          regionsPlugin.addRegion({
            start: msg.detection_end,
            end: duration,
            color: 'rgba(0, 0, 0, 0.35)',
            drag: false,
            resize: false
          });
        }
        // Detektionsfenster hell markieren
        regionsPlugin.addRegion({
          start: msg.detection_start,
          end: msg.detection_end,
          color: 'rgba(51, 122, 183, 0.15)',
          drag: false,
          resize: false
        });
      }

      // Seek & Play
      if (msg.seek != null && msg.seek > 0) {
        ws.setTime(msg.seek);
      }
      ws.play();
    });

    // Klick auf Waveform → Play/Pause
    ws.on('interaction', function() {
      ws.playPause();
    });

    // Laden
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
