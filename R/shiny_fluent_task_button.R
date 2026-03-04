#' @export

task_button_js <- "
(function() {
  var style = document.createElement('style');
  style.textContent = '@keyframes task-spin { to { transform: rotate(360deg); } }';
  document.head.appendChild(style);

  Shiny.addCustomMessageHandler('taskBtnBusy', function(msg) {
    var wrapper = document.getElementById(msg.id);
    if (!wrapper) return;
    wrapper.setAttribute('data-state', 'busy');
    var fluentBtn = wrapper.querySelector('button');
    if (fluentBtn) fluentBtn.disabled = true;
    var fluentLabel = wrapper.querySelector('.ms-Button-label');
    if (fluentLabel) fluentLabel.textContent = msg.label_busy;
    var icon = wrapper.querySelector('.ms-Button-icon');
    if (icon) {
      icon.dataset.originalContent = icon.innerHTML;
      icon.innerHTML = '<span style=\"display:inline-block;width:14px;height:14px;border:2px solid rgba(255,255,255,0.4);border-top-color:#fff;border-radius:50%;animation:task-spin 0.8s linear infinite;\"></span>';
    }
  });

  Shiny.addCustomMessageHandler('taskBtnReady', function(msg) {
    var wrapper = document.getElementById(msg.id);
    if (!wrapper) return;
    wrapper.setAttribute('data-state', 'ready');
    var fluentBtn = wrapper.querySelector('button');
    if (fluentBtn) fluentBtn.disabled = false;
    var fluentLabel = wrapper.querySelector('.ms-Button-label');
    var labelReady  = wrapper.querySelector('.task-btn-label-ready');
    if (fluentLabel && labelReady) fluentLabel.textContent = labelReady.textContent;
    var icon = wrapper.querySelector('.ms-Button-icon');
    if (icon && icon.dataset.originalContent) {
      icon.innerHTML = icon.dataset.originalContent;
    }
  });

})();
"
#' @export

task_button_css <- "
@keyframes task-spin {
  to { transform: rotate(360deg); }
}

.task-btn-spinner {
  display: none;
  width: 14px;
  height: 14px;
  border: 2px solid rgba(255,255,255,0.3);
  border-top-color: #fff;
  border-radius: 50%;
  animation: task-spin 0.8s linear infinite;
  margin-right: 6px;
  vertical-align: middle;
}

[data-state='busy'] .task-btn-spinner  { display: inline-block; }
[data-state='ready'] .task-btn-spinner { display: none; }

 .notification-overlay {
               position: fixed;
               top: 0;
              left: 0;
              width: 100%;
              height: 100%;
              background-color: rgba(0, 0, 0, 0.5);
              z-index: 99998;
              display: none;
              opacity: 0;
              transition: opacity 0.3s ease-in-out;
            }
      
              .notification-overlay.show {
                display: block;
                opacity: 1;
            }
      
              #shiny-notification-panel {
                position: fixed;
                top: 50% !important;
                left: 50% !important;
                right: auto !important;
                bottom: auto !important;
                transform: translate(-50%, -50%) !important;
                width: auto;
                max-width: 500px;
                z-index: 99999;
            }
      
          @keyframes slideDown {
           from {
             opacity: 0;
             transform: translate(-50%, -60%);
            }
           to {
             opacity: 1;
             transform: translate(-50%, -50%);
           }
         }
      
           @keyframes pulse {
             0%, 100% {
              transform: scale(1);
            }
             50% {
          transform: scale(1.05);
             }
          }
      
             .shiny-notification {
                  background-color: white;
                  border: none;
                  border-radius: 12px;
                  box-shadow: 0 8px 32px rgba(0, 0, 0, 0.2);
                  padding: 40px 30px 30px 30px;
                  text-align: center;
                  font-size: 16px;
                  color: #333;
                   opacity: 1 !important;
                   min-width: 350px;
                  position: relative;
                  animation: slideDown 0.4s ease-out;
              }

              .shiny-notification::before {
                  content: '';
                  position: absolute;
                  top: 0;
                  left: 0;
                  right: 0;
                  height: 4px;
                  border-radius: 12px 12px 0 0;
                  background: linear-gradient(90deg, #3498db, #2980b9);
              }
      
              .shiny-notification-message::before {
                 background: linear-gradient(90deg, #3498db, #2980b9);
                }
      
              .shiny-notification-warning::before {
                 background: linear-gradient(90deg, #f39c12, #e67e22);
                }
      
              .shiny-notification-error::before {
                 background: linear-gradient(90deg, #e74c3c, #c0392b);
                }
      
             .shiny-notification.success-notification::before {
                background: linear-gradient(90deg, #27ae60, #229954);
              }
      
              .shiny-notification-message,
              .shiny-notification-warning,
              .shiny-notification-error {
                 background-color: white;
                 border-left: none;
              }
      
            .shiny-notification .message-text {
               color: #555;
               line-height: 1.6;
               margin-top: 8px;
               font-size: 15px;
            }"

#' Task button wrapper for shiny.fluent
#'
#' Wraps a \code{PrimaryButton.shinyInput} with busy/ready state management.
#'
#' @param inputId `chr` The input that will be used to access the value.
#' @param label `chr` The button label.
#' @param ... Additional arguments passed to \code{PrimaryButton.shinyInput}.
#'
#' @return A \code{tagList} containing the button with CSS and JS handlers.
#' @noRd
#' @export

taskButton <- function(inputId, label, label_busy = "Processing...", ...) {
  div(
    id = paste0(inputId, "_wrapper"),
    `data-state` = "ready",
    tags$span(class = "task-btn-label-ready", style = "display:none;", label),
    tags$span(class = "task-btn-label-busy",  style = "display:none;", label_busy),
    PrimaryButton.shinyInput(
      inputId = inputId,
      text = label,
      ...
    )
  )
}

#' Server-side handler for task button busy/ready states
#'
#' Use inside an \code{observeEvent} to toggle a \code{taskButton}
#' between busy and ready states around a long-running expression.
#'
#' @param session Shiny session object.
#' @param inputId `chr` The input ID of the target \code{taskButton}.
#' @param expr Expression to evaluate while the button is in busy state.
#' @param label_busy `chr` Label shown during processing. Defaults to \code{"Processing..."}.
#'
#' @return Invisibly returns the result of \code{expr}.
#' @noRd
#' @export

withTaskButton <- function(session, inputId, expr,
                           label_busy = "Processing...") {
  session$sendCustomMessage("taskBtnBusy", list(
    id    = paste0(inputId, "_wrapper"),
    label_busy = label_busy
  ))

  result <- force(expr)

  session$sendCustomMessage("taskBtnReady", list(
    id = paste0(inputId, "_wrapper")
  ))

  invisible(result)
}