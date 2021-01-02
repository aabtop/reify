#ifndef WEB_INTERFACE_H_
#define WEB_INTERFACE_H_

#include <qobject.h>
#include <qwidget.h>

class WebInterface : public QObject {
  Q_OBJECT

 public:
  WebInterface(QWidget* parent = Q_NULLPTR);

  void ChangeData(const QString& data);

 public slots:
  void generateRandomData();

 signals:
  void dataChanged(const QString& data);
};

#endif  // WEB_INTERFACE_H_
