import React from "react";
import "./Dialog.css";
import classNames from "classnames";
import { ClassValue } from "classnames/types";

export interface Props {
  isShown: boolean;
  className?: ClassValue[] | ClassValue;
}

export interface State {}

export class Dialog extends React.PureComponent<Props, State> {
  render() {
    const { isShown } = this.props;
    if (!isShown) {
      return null;
    }

    const { className } = this.props;

    return (
      <div className={classNames("dialog", className)}>
        <div className="dialog__main">{this.props.children}</div>
      </div>
    );
  }
}
