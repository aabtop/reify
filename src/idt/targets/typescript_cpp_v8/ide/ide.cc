#include "reify/typescript_cpp_v8/ide.h"

#include <QApplication>
#include <QPalette>
#include <QWebEngineView>
#include <Qt>

#include "main_window.h"

namespace reify {
namespace typescript_cpp_v8 {

namespace {
QPalette DarkModePalette() {
  auto palette = QPalette();
  palette.setColor(QPalette::Window, QColor(53, 53, 53));
  palette.setColor(QPalette::WindowText, QColor(255, 255, 255));
  palette.setColor(QPalette::Base, QColor(40, 40, 40));
  palette.setColor(QPalette::AlternateBase, QColor(53, 53, 53));
  palette.setColor(QPalette::ToolTipBase, QColor(40, 40, 40));
  palette.setColor(QPalette::ToolTipText, QColor(255, 255, 255));
  palette.setColor(QPalette::Text, QColor(255, 255, 255));
  palette.setColor(QPalette::Button, QColor(53, 53, 53));
  palette.setColor(QPalette::ButtonText, QColor(255, 255, 255));
  palette.setColor(QPalette::BrightText, QColor(255, 0, 0));
  palette.setColor(QPalette::Link, QColor(42, 130, 218));
  palette.setColor(QPalette::Highlight, QColor(42, 160, 160));
  palette.setColor(QPalette::HighlightedText, QColor(0, 0, 0));
  palette.setColor(QPalette::Light, QColor(160, 160, 160));
  palette.setColor(QPalette::Midlight, QColor(140, 140, 140));
  palette.setColor(QPalette::Mid, QColor(120, 120, 120));
  palette.setColor(QPalette::Dark, QColor(80, 80, 80));
  return palette;
}
}  // namespace

int StartIdeWindow(const std::string& window_title,
                   DomainVisualizer* domain_visualizer) {
  QCoreApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
  int argc;
  QApplication a(argc, nullptr);

  a.setPalette(DarkModePalette());

  ide::MainWindow w(window_title, domain_visualizer);
  w.show();
  return a.exec();
}

}  // namespace typescript_cpp_v8
}  // namespace reify