import React from "react";
import "./DialogBody.css";
import classNames from "classnames";
import { ClassValue } from "classnames/types";

export interface Props {
  className?: ClassValue[] | ClassValue;
}

export interface State {}

export class DialogBody extends React.PureComponent<Props, State> {
  render() {
    const { className } = this.props;

    return (
      <div className={classNames("dialog__body", className)}>
        {this.props.children}
      </div>
    );
  }
}
