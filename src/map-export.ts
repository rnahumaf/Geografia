export type ExportFormat = "png" | "jpeg" | "pdf";

export async function exportMap(
  container: HTMLElement,
  format: ExportFormat,
): Promise<void> {
  const { default: html2canvas } = await import("html2canvas");
  const canvas = await html2canvas(container, {
    useCORS: true,
    allowTaint: false,
    backgroundColor: "#eef1ef",
    logging: false,
    scale: Math.min(2, Math.max(1, window.devicePixelRatio)),
    ignoreElements: (element) =>
      element.classList.contains("export-toolbar") ||
      element.classList.contains("map-status"),
  });
  const timestamp = new Date().toISOString().slice(0, 19).replaceAll(":", "-");
  const baseName = `risco-itupararanga-${timestamp}`;

  if (format === "pdf") {
    await exportPdf(canvas, `${baseName}.pdf`);
    return;
  }
  const mimeType = format === "jpeg" ? "image/jpeg" : "image/png";
  const extension = format === "jpeg" ? "jpg" : "png";
  const blob = await canvasToBlob(canvas, mimeType, 0.94);
  downloadBlob(blob, `${baseName}.${extension}`);
}

async function exportPdf(canvas: HTMLCanvasElement, filename: string): Promise<void> {
  const { jsPDF } = await import("jspdf");
  const pdf = new jsPDF({ orientation: "landscape", unit: "mm", format: "a4" });
  const pageWidth = pdf.internal.pageSize.getWidth();
  const pageHeight = pdf.internal.pageSize.getHeight();
  const margin = 10;
  const titleY = 12;
  const imageTop = 20;
  const footerHeight = 12;
  const maxWidth = pageWidth - margin * 2;
  const maxHeight = pageHeight - imageTop - footerHeight;
  const ratio = Math.min(maxWidth / canvas.width, maxHeight / canvas.height);
  const width = canvas.width * ratio;
  const height = canvas.height * ratio;
  const x = (pageWidth - width) / 2;

  pdf.setFont("helvetica", "bold");
  pdf.setFontSize(12);
  pdf.text("Mapa exploratório de risco - ruptura de Itupararanga", margin, titleY);
  pdf.addImage(canvas.toDataURL("image/jpeg", 0.94), "JPEG", x, imageTop, width, height);
  pdf.setFont("helvetica", "normal");
  pdf.setFontSize(7.5);
  pdf.setTextColor(78, 91, 84);
  pdf.text(
    "Estudo experimental não calibrado. Não substitui o PAE nem orienta decisões operacionais.",
    margin,
    pageHeight - 5,
  );
  pdf.save(filename);
}

function canvasToBlob(
  canvas: HTMLCanvasElement,
  mimeType: string,
  quality: number,
): Promise<Blob> {
  return new Promise((resolve, reject) => {
    canvas.toBlob(
      (blob) => (blob ? resolve(blob) : reject(new Error("Falha ao codificar a imagem."))),
      mimeType,
      quality,
    );
  });
}

function downloadBlob(blob: Blob, filename: string): void {
  const url = URL.createObjectURL(blob);
  const link = document.createElement("a");
  link.href = url;
  link.download = filename;
  link.click();
  window.setTimeout(() => URL.revokeObjectURL(url), 1_000);
}
