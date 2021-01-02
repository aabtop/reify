#include "src/ide/about_dialog.h"

#include "src/ide/ui_about_dialog.h"

AboutDialog::AboutDialog(QWidget* parent)
    : QDialog(parent, Qt::WindowTitleHint | Qt::WindowCloseButtonHint),
      ui(new Ui::AboutDialog) {
  ui->setupUi(this);

  connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(OnFinished()));
}

AboutDialog::~AboutDialog() { delete ui; }

void AboutDialog::OnFinished() {
  // No need to do anything here, base logic already closes the dialog when
  // the button is pressed.
}
