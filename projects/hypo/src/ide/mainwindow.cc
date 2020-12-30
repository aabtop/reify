#include "src/ide/mainwindow.h"

#include <qwebchannel.h>
#include <qwebengineview.h>

#include "src/ide/ui_mainwindow.h"
#include "src/ide/web_interface.h"

MainWindow::MainWindow(QWidget* parent)
    : QMainWindow(parent), ui(new Ui::MainWindow) {
  ui->setupUi(this);

  QWebChannel* channel = new QWebChannel(ui->editor->page());
  ui->editor->page()->setWebChannel(channel);

  WebInterface* test_object = new WebInterface(this);
  channel->registerObject(QStringLiteral("test_object"), test_object);

  ui->editor->load(QUrl("qrc:/src/ide/index.html"));
  ui->editor->show();

  connect(ui->pushButton, SIGNAL(clicked()), test_object,
          SLOT(generateRandomData()));
}

MainWindow::~MainWindow() { delete ui; }
